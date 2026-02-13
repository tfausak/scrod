-- | Convert a parsed GHC AST into Scrod's core representation.
--
-- This module provides the main 'fromGhc' entry point and the
-- declaration dispatch logic. The actual conversion of individual
-- declaration types is delegated to submodules under
-- @Scrod.Convert.FromGhc.*@.
module Scrod.Convert.FromGhc where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
import qualified Data.Version
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified GHC.Data.FastString as FastString
import qualified GHC.Driver.DynFlags as DynFlags
import qualified GHC.Driver.Session as Session
import qualified GHC.Hs as Hs
import qualified GHC.Hs.Doc as HsDoc
import qualified GHC.Hs.DocString as DocString
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.LanguageExtensions.Type as GhcExtension
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Types.PkgQual as PkgQual
import qualified GHC.Types.SourceText as SourceText
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Syntax
import qualified Numeric.Natural as Natural
import qualified PackageInfo_scrod as PackageInfo
import qualified Scrod.Convert.FromGhc.Constructors as Constructors
import qualified Scrod.Convert.FromGhc.Doc as GhcDoc
import qualified Scrod.Convert.FromGhc.Exports as Exports
import qualified Scrod.Convert.FromGhc.InstanceParents as InstanceParents
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Convert.FromGhc.ItemKind as ItemKindFrom
import qualified Scrod.Convert.FromGhc.Merge as Merge
import qualified Scrod.Convert.FromGhc.Names as Names
import qualified Scrod.Convert.FromHaddock as FromHaddock
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Extension as Extension
import qualified Scrod.Core.Import as Import
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Language as Language
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.Module as Module
import qualified Scrod.Core.ModuleName as ModuleName
import qualified Scrod.Core.PackageName as PackageName
import qualified Scrod.Core.Since as Since
import qualified Scrod.Core.Version as Version
import qualified Scrod.Core.Warning as Warning
import qualified Scrod.Ghc.OnOff as OnOff

-- | Convert a parsed GHC module to the internal 'Module' type.
fromGhc ::
  Bool ->
  ( (Maybe Session.Language, [DynFlags.OnOff GhcExtension.Extension]),
    SrcLoc.Located (Hs.HsModule Ghc.GhcPs)
  ) ->
  Either String Module.Module
fromGhc isSignature ((language, extensions), lHsModule) = do
  version <- maybe (Left "invalid version") Right $ versionFromBase PackageInfo.version
  let (moduleDocumentation, moduleSince) = extractModuleDocAndSince lHsModule
  Right
    Module.MkModule
      { Module.version = version,
        Module.language = languageFromGhc <$> language,
        Module.extensions = extensionsToMap extensions,
        Module.documentation = moduleDocumentation,
        Module.since = moduleSince,
        Module.signature = isSignature,
        Module.name = extractModuleName lHsModule,
        Module.warning = extractModuleWarning lHsModule,
        Module.exports = Exports.extractModuleExports lHsModule,
        Module.imports = extractModuleImports lHsModule,
        Module.items = extractItems lHsModule
      }

-- | Convert base version to our 'Version' type.
versionFromBase :: Data.Version.Version -> Maybe Version.Version
versionFromBase v = case Data.Version.versionBranch v of
  [] -> Nothing
  x : xs -> (Just . Version.MkVersion) . fmap fromIntegral $ x NonEmpty.:| xs

-- | Convert GHC language to our 'Language' type.
languageFromGhc :: Session.Language -> Language.Language
languageFromGhc lang =
  Language.MkLanguage . Text.pack $ case lang of
    Session.Haskell98 -> "Haskell98"
    Session.Haskell2010 -> "Haskell2010"
    Session.GHC2021 -> "GHC2021"
    Session.GHC2024 -> "GHC2024"

-- | Convert GHC extension to our 'Extension' type.
extensionFromGhc :: GhcExtension.Extension -> Extension.Extension
extensionFromGhc =
  Extension.MkExtension
    . Text.pack
    . Outputable.showSDocUnsafe
    . Outputable.ppr

-- | Convert list of 'OnOff' extensions to a 'Map'.
extensionsToMap ::
  [DynFlags.OnOff GhcExtension.Extension] ->
  Map.Map Extension.Extension Bool
extensionsToMap =
  Map.fromListWith (\_ x -> x)
    . fmap (Tuple.swap . fmap extensionFromGhc . OnOff.onOff ((,) True) ((,) False))

-- | Extract module name from the parsed module.
extractModuleName ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe (Located.Located ModuleName.ModuleName)
extractModuleName lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
  lModuleName <- Syntax.hsmodName hsModule
  let srcSpan = Annotation.getLocA lModuleName
      moduleName = Internal.moduleNameFromGhc $ SrcLoc.unLoc lModuleName
  Internal.locatedFromGhc $ SrcLoc.L srcSpan moduleName

-- | Extract module documentation and @since information from the parsed module.
-- Parses the Haddock MetaDoc once and extracts both the Doc and Since.
extractModuleDocAndSince ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  (Doc.Doc, Maybe Since.Since)
extractModuleDocAndSince lHsModule =
  case extractRawDocString lHsModule of
    Nothing -> (Doc.Empty, Nothing)
    Just rawDocString ->
      let metaDoc :: Haddock.MetaDoc m Haddock.Identifier
          metaDoc = Haddock.parseParas Nothing rawDocString
          doc = FromHaddock.fromHaddock $ Haddock._doc metaDoc
          since = Haddock._metaSince (Haddock._meta metaDoc) >>= metaSinceToSince
       in (doc, since)

-- | Extract raw documentation string from the module header.
extractRawDocString ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe String
extractRawDocString lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
      xModulePs = Syntax.hsmodExt hsModule
  lHsDoc <- Hs.hsmodHaddockModHeader xModulePs
  let hsDoc = SrcLoc.unLoc lHsDoc
      hsDocString = HsDoc.hsDocString hsDoc
  Just $ DocString.renderHsDocString hsDocString

-- | Convert a Haddock MetaSince to our 'Since'.
metaSinceToSince :: Haddock.MetaSince -> Maybe Since.Since
metaSinceToSince metaSince = do
  versionNE <- NonEmpty.nonEmpty $ Haddock.sinceVersion metaSince
  Just
    Since.MkSince
      { Since.package =
          PackageName.MkPackageName . Text.pack
            <$> Haddock.sincePackage metaSince,
        Since.version =
          Version.MkVersion $ fmap (fromIntegral :: Int -> Natural.Natural) versionNE
      }

-- | Extract module deprecation warning.
extractModuleWarning ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe Warning.Warning
extractModuleWarning lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
      xModulePs = Syntax.hsmodExt hsModule
  lWarningTxt <- Hs.hsmodDeprecMessage xModulePs
  let warningTxt = SrcLoc.unLoc lWarningTxt
  Just $ Internal.warningTxtToWarning warningTxt

-- | Extract module imports.
extractModuleImports ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  [Import.Import]
extractModuleImports lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
   in convertImportDecl <$> Syntax.hsmodImports hsModule

-- | Convert a GHC import declaration to our 'Import' type.
convertImportDecl ::
  SrcLoc.GenLocated l (Syntax.ImportDecl Ghc.GhcPs) ->
  Import.Import
convertImportDecl lImportDecl =
  let importDecl = SrcLoc.unLoc lImportDecl
   in Import.MkImport
        { Import.name = Internal.moduleNameFromGhc . SrcLoc.unLoc $ Syntax.ideclName importDecl,
          Import.package = packageFromPkgQual $ Syntax.ideclPkgQual importDecl,
          Import.alias = Internal.moduleNameFromGhc . SrcLoc.unLoc <$> Syntax.ideclAs importDecl
        }

-- | Convert a GHC package qualifier to our 'PackageName' type.
packageFromPkgQual :: PkgQual.RawPkgQual -> Maybe PackageName.PackageName
packageFromPkgQual pkgQual = case pkgQual of
  PkgQual.NoRawPkgQual -> Nothing
  PkgQual.RawPkgQual sl ->
    Just . PackageName.MkPackageName . Text.pack . FastString.unpackFS $ SourceText.sl_fs sl

-- | Extract items from the module.
extractItems ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  [Located.Located Item.Item]
extractItems lHsModule =
  let rawItems = Internal.runConvert $ extractItemsM lHsModule
      instanceHeadTypes = InstanceParents.extractInstanceHeadTypeNames lHsModule
      parentedItems = InstanceParents.associateInstanceParents instanceHeadTypes rawItems
   in Merge.mergeItemsByName parentedItems

-- | Extract items in the conversion monad.
extractItemsM ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Internal.ConvertM [Located.Located Item.Item]
extractItemsM lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
      declsWithDocs = GhcDoc.associateDocs decls
  concat <$> traverse (uncurry convertDeclWithDocMaybeM) declsWithDocs

-- | Convert a declaration with documentation.
convertDeclWithDocMaybeM ::
  Doc.Doc ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertDeclWithDocMaybeM doc lDecl = case SrcLoc.unLoc lDecl of
  Syntax.TyClD _ tyClDecl -> convertTyClDeclWithDocM doc lDecl tyClDecl
  Syntax.RuleD _ ruleDecls -> convertRuleDeclsM ruleDecls
  Syntax.DocD {} -> Maybe.maybeToList <$> convertDeclSimpleM lDecl
  Syntax.SigD _ sig -> convertSigDeclM doc lDecl sig
  Syntax.KindSigD _ kindSig ->
    let sig = Just $ Names.extractKindSigSignature kindSig
     in Maybe.maybeToList <$> convertDeclWithDocM Nothing doc (Just $ Names.extractStandaloneKindSigName kindSig) sig lDecl
  Syntax.InstD _ inst -> convertInstDeclWithDocM doc lDecl inst
  Syntax.ForD _ foreignDecl ->
    let name = Just $ Names.extractForeignDeclName foreignDecl
        sig = Just $ Names.extractForeignDeclSignature foreignDecl
     in Maybe.maybeToList <$> convertDeclWithDocM Nothing doc name sig lDecl
  Syntax.SpliceD _ spliceDecl ->
    let sig = Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ spliceDecl
     in Maybe.maybeToList <$> convertDeclWithDocM Nothing doc Nothing sig lDecl
  Syntax.DefD {} -> pure []
  _ -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc (Names.extractDeclName lDecl) Nothing lDecl

-- | Convert a type/class declaration with documentation.
convertTyClDeclWithDocM ::
  Doc.Doc ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Syntax.TyClDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertTyClDeclWithDocM doc lDecl tyClDecl = case tyClDecl of
  Syntax.FamDecl _ famDecl -> case Syntax.fdInfo famDecl of
    Syntax.ClosedTypeFamily (Just eqns) -> do
      parentItem <- convertDeclWithDocM Nothing doc (Names.extractTyClDeclName tyClDecl) Nothing lDecl
      let parentKey = fmap (Item.key . Located.value) parentItem
      eqnItems <- convertTyFamInstEqnsM parentKey eqns
      pure $ Maybe.maybeToList parentItem <> eqnItems
    _ -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc (Names.extractTyClDeclName tyClDecl) Nothing lDecl
  Syntax.DataDecl _ _ _ _ dataDefn -> do
    parentItem <- convertDeclWithDocM Nothing doc (Names.extractTyClDeclName tyClDecl) (Names.extractTyClDeclTyVars tyClDecl) lDecl
    let parentKey = fmap (Item.key . Located.value) parentItem
        parentType = Names.extractParentTypeText tyClDecl
    childItems <- convertDataDefnM parentKey parentType dataDefn
    pure $ Maybe.maybeToList parentItem <> childItems
  Syntax.ClassDecl {Syntax.tcdSigs = sigs, Syntax.tcdATs = ats, Syntax.tcdDocs = docs} -> do
    parentItem <- convertDeclWithDocM Nothing doc (Names.extractTyClDeclName tyClDecl) (Names.extractTyClDeclTyVars tyClDecl) lDecl
    let parentKey = fmap (Item.key . Located.value) parentItem
    methodItems <- convertClassSigsWithDocsM parentKey sigs docs
    familyItems <- convertFamilyDeclsM parentKey ats
    pure $ Maybe.maybeToList parentItem <> methodItems <> familyItems
  Syntax.SynDecl {} -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc (Names.extractTyClDeclName tyClDecl) (Names.extractSynDeclSignature tyClDecl) lDecl

-- | Convert an instance declaration with documentation.
convertInstDeclWithDocM ::
  Doc.Doc ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Syntax.InstDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertInstDeclWithDocM doc lDecl inst = case inst of
  Syntax.DataFamInstD _ dataFamInst -> do
    parentItem <- convertDeclWithDocM Nothing doc (Names.extractInstDeclName inst) Nothing lDecl
    let parentKey = fmap (Item.key . Located.value) parentItem
    childItems <- convertDataDefnM parentKey Nothing (Syntax.feqn_rhs $ Syntax.dfid_eqn dataFamInst)
    pure $ Maybe.maybeToList parentItem <> childItems
  _ -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc (Names.extractInstDeclName inst) Nothing lDecl

-- | Convert a signature declaration.
convertSigDeclM ::
  Doc.Doc ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Syntax.Sig Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertSigDeclM doc lDecl sig = case sig of
  Syntax.TypeSig _ names _ ->
    let sigText = Names.extractSigSignature sig
     in Maybe.catMaybes <$> traverse (convertSigNameM doc sigText) names
  Syntax.PatSynSig _ names _ ->
    let sigText = Names.extractSigSignature sig
     in Maybe.catMaybes <$> traverse (convertSigNameM doc sigText) names
  _ -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc (Names.extractSigName sig) Nothing lDecl

-- | Convert a single name from a signature.
convertSigNameM ::
  Doc.Doc ->
  Maybe Text.Text ->
  Syntax.LIdP Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertSigNameM doc sig lName =
  Internal.mkItemM (Annotation.getLocA lName) Nothing (Just $ Internal.extractIdPName lName) doc sig ItemKind.Function

-- | Convert a simple declaration without special handling.
convertDeclSimpleM ::
  Syntax.LHsDecl Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertDeclSimpleM = convertDeclWithDocM Nothing Doc.Empty Nothing Nothing

-- | Convert a declaration with documentation.
convertDeclWithDocM ::
  Maybe ItemKey.ItemKey ->
  Doc.Doc ->
  Maybe ItemName.ItemName ->
  Maybe Text.Text ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertDeclWithDocM parentKey doc itemName sig lDecl =
  let itemKind = ItemKindFrom.itemKindFromDecl $ SrcLoc.unLoc lDecl
   in Internal.mkItemM (Annotation.getLocA lDecl) parentKey itemName doc sig itemKind

-- | Convert rule declarations.
convertRuleDeclsM ::
  Syntax.RuleDecls Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertRuleDeclsM (Syntax.HsRules _ rules) = Maybe.catMaybes <$> traverse convertRuleDeclM rules

-- | Convert a single rule declaration.
convertRuleDeclM ::
  Syntax.LRuleDecl Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertRuleDeclM lRuleDecl =
  Internal.mkItemM (Annotation.getLocA lRuleDecl) Nothing Nothing Doc.Empty Nothing ItemKind.Rule

-- | Convert class signatures with associated documentation.
convertClassSigsWithDocsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.LSig Ghc.GhcPs] ->
  [Hs.LDocDecl Ghc.GhcPs] ->
  Internal.ConvertM [Located.Located Item.Item]
convertClassSigsWithDocsM parentKey sigs docs =
  let classOpSigs = filter isClassOpSig sigs
      sigDecls = fmap (fmap (Syntax.SigD Hs.noExtField)) classOpSigs
      docDecls = fmap (fmap (Syntax.DocD Hs.noExtField)) docs
      allDecls = List.sortBy (\a b -> SrcLoc.leftmost_smallest (Annotation.getLocA a) (Annotation.getLocA b)) (sigDecls <> docDecls)
      sigsWithDocs = GhcDoc.associateDocs allDecls
   in concat <$> traverse (uncurry (convertClassDeclWithDocM parentKey)) sigsWithDocs
  where
    isClassOpSig :: Syntax.LSig Ghc.GhcPs -> Bool
    isClassOpSig lSig = case SrcLoc.unLoc lSig of
      Syntax.ClassOpSig {} -> True
      _ -> False

-- | Convert a class body declaration with associated documentation.
convertClassDeclWithDocM ::
  Maybe ItemKey.ItemKey ->
  Doc.Doc ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertClassDeclWithDocM parentKey doc lDecl = case SrcLoc.unLoc lDecl of
  Syntax.SigD _ sig -> case sig of
    Syntax.ClassOpSig _ _ names _ ->
      let sigText = Names.extractSigSignature sig
       in Maybe.catMaybes <$> traverse (convertIdPM parentKey doc sigText) names
    _ -> pure []
  _ -> pure []

-- | Convert an identifier with parent key, documentation, and signature.
convertIdPM ::
  Maybe ItemKey.ItemKey ->
  Doc.Doc ->
  Maybe Text.Text ->
  Syntax.LIdP Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertIdPM parentKey doc sig lIdP =
  Internal.mkItemM (Annotation.getLocA lIdP) parentKey (Just $ Internal.extractIdPName lIdP) doc sig ItemKind.ClassMethod

-- | Convert family declarations.
convertFamilyDeclsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.LFamilyDecl Ghc.GhcPs] ->
  Internal.ConvertM [Located.Located Item.Item]
convertFamilyDeclsM parentKey = fmap Maybe.catMaybes . traverse (convertFamilyDeclM parentKey)

-- | Convert a single family declaration.
convertFamilyDeclM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LFamilyDecl Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertFamilyDeclM parentKey lFamilyDecl =
  let famDecl = SrcLoc.unLoc lFamilyDecl
      itemKind = ItemKindFrom.itemKindFromFamilyDecl famDecl
   in Internal.mkItemM
        (Annotation.getLocA lFamilyDecl)
        parentKey
        (Just $ Names.extractFamilyDeclName famDecl)
        Doc.Empty
        Nothing
        itemKind

-- | Convert type family instance equations.
convertTyFamInstEqnsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.LTyFamInstEqn Ghc.GhcPs] ->
  Internal.ConvertM [Located.Located Item.Item]
convertTyFamInstEqnsM parentKey = fmap Maybe.catMaybes . traverse (convertTyFamInstEqnM parentKey)

-- | Convert a single type family instance equation.
convertTyFamInstEqnM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LTyFamInstEqn Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertTyFamInstEqnM parentKey lEqn =
  let eqn = SrcLoc.unLoc lEqn
      sig = Just . Text.pack . Outputable.showSDocUnsafe $ extractTyFamInstEqnSig eqn
   in Internal.mkItemM (Annotation.getLocA lEqn) parentKey Nothing Doc.Empty sig ItemKind.TypeFamilyInstance

-- | Pretty-print a type family instance equation.
extractTyFamInstEqnSig :: Syntax.TyFamInstEqn Ghc.GhcPs -> Outputable.SDoc
extractTyFamInstEqnSig eqn =
  Outputable.ppr (Syntax.feqn_tycon eqn)
    Outputable.<+> Outputable.hsep (Outputable.ppr <$> Syntax.feqn_pats eqn)
    Outputable.<+> Outputable.text "="
    Outputable.<+> Outputable.ppr (Syntax.feqn_rhs eqn)

-- | Convert data definition constructors and deriving clauses.
convertDataDefnM ::
  Maybe ItemKey.ItemKey ->
  Maybe Text.Text ->
  Syntax.HsDataDefn Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertDataDefnM parentKey parentType dataDefn = do
  conItems <- concat <$> (traverse (Constructors.convertConDeclM parentKey parentType) . dataDefnConsList $ Syntax.dd_cons dataDefn)
  derivItems <- convertDerivingClausesM parentKey $ Syntax.dd_derivs dataDefn
  pure $ conItems <> derivItems

-- | Convert DataDefnCons to a list.
dataDefnConsList :: Syntax.DataDefnCons a -> [a]
dataDefnConsList ddc = case ddc of
  Syntax.NewTypeCon con -> [con]
  Syntax.DataTypeCons _ cons -> cons

-- | Convert deriving clauses.
convertDerivingClausesM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsDeriving Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertDerivingClausesM parentKey = fmap concat . traverse (convertDerivingClauseM parentKey)

-- | Convert a single deriving clause.
convertDerivingClauseM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LHsDerivingClause Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertDerivingClauseM parentKey lClause = do
  let clause = SrcLoc.unLoc lClause
      derivClauseTys = SrcLoc.unLoc $ Syntax.deriv_clause_tys clause
  convertDerivClauseTysM parentKey derivClauseTys

-- | Convert deriving clause types.
convertDerivClauseTysM ::
  Maybe ItemKey.ItemKey ->
  Syntax.DerivClauseTys Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertDerivClauseTysM parentKey dct = case dct of
  Syntax.DctSingle _ lSigTy -> Maybe.maybeToList <$> convertDerivedTypeM parentKey lSigTy
  Syntax.DctMulti _ lSigTys -> Maybe.catMaybes <$> traverse (convertDerivedTypeM parentKey) lSigTys

-- | Convert a derived type to an item.
convertDerivedTypeM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LHsSigType Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertDerivedTypeM parentKey lSigTy =
  Internal.mkItemM (Annotation.getLocA lSigTy) parentKey (extractDerivedTypeName lSigTy) (extractDerivedTypeDoc lSigTy) Nothing ItemKind.DerivedInstance

-- | Extract name from a derived type.
extractDerivedTypeName :: Syntax.LHsSigType Ghc.GhcPs -> Maybe ItemName.ItemName
extractDerivedTypeName lSigTy =
  let sigTy = SrcLoc.unLoc lSigTy
      bodyTy = SrcLoc.unLoc $ Syntax.sig_body sigTy
      ty = case bodyTy of
        Syntax.HsDocTy _ lTy _ -> SrcLoc.unLoc lTy
        _ -> bodyTy
   in Just . ItemName.MkItemName . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ ty

-- | Extract documentation from a derived type.
extractDerivedTypeDoc :: Syntax.LHsSigType Ghc.GhcPs -> Doc.Doc
extractDerivedTypeDoc lSigTy =
  let sigTy = SrcLoc.unLoc lSigTy
      bodyTy = SrcLoc.unLoc $ Syntax.sig_body sigTy
   in case bodyTy of
        Syntax.HsDocTy _ _ lDoc -> GhcDoc.convertLHsDoc lDoc
        _ -> Doc.Empty
