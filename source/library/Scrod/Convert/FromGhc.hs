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
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable
import qualified Data.Tuple as Tuple
import qualified Data.Version
import qualified GHC.Data.FastString as FastString
import qualified GHC.Driver.DynFlags as DynFlags
import qualified GHC.Driver.Session as Session
import qualified GHC.Hs as Hs
import qualified GHC.Hs.Doc as HsDoc
import qualified GHC.Hs.DocString as DocString
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.LanguageExtensions.Type as GhcExtension
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Types.Basic as Basic
import qualified GHC.Types.PkgQual as PkgQual
import qualified GHC.Types.SourceText as SourceText
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Syntax
import qualified Language.Haskell.Syntax.Basic as SyntaxBasic
import qualified PackageInfo_scrod as PackageInfo
import qualified Scrod.Convert.FromGhc.CompleteParents as CompleteParents
import qualified Scrod.Convert.FromGhc.Constructors as Constructors
import qualified Scrod.Convert.FromGhc.Doc as GhcDoc
import qualified Scrod.Convert.FromGhc.Exports as Exports
import qualified Scrod.Convert.FromGhc.FamilyInstanceParents as FamilyInstanceParents
import qualified Scrod.Convert.FromGhc.FixityParents as FixityParents
import qualified Scrod.Convert.FromGhc.InlineParents as InlineParents
import qualified Scrod.Convert.FromGhc.InstanceParents as InstanceParents
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Convert.FromGhc.ItemKind as ItemKindFrom
import qualified Scrod.Convert.FromGhc.KindSigParents as KindSigParents
import qualified Scrod.Convert.FromGhc.Merge as Merge
import qualified Scrod.Convert.FromGhc.Names as Names
import qualified Scrod.Convert.FromGhc.ParentAssociation as ParentAssociation
import qualified Scrod.Convert.FromGhc.RoleParents as RoleParents
import qualified Scrod.Convert.FromGhc.SpecialiseParents as SpecialiseParents
import qualified Scrod.Convert.FromGhc.WarningParents as WarningParents
import qualified Scrod.Core.Category as Category
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Export as Export
import qualified Scrod.Core.ExportIdentifier as ExportIdentifier
import qualified Scrod.Core.ExportName as ExportName
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
import qualified Scrod.Core.Subordinates as Subordinates
import qualified Scrod.Core.Version as Version
import qualified Scrod.Core.Visibility as Visibility
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
      namedDocChunks = extractNamedDocChunks lHsModule
      rawExports = Exports.extractModuleExports lHsModule
      referencedChunkNames = extractReferencedChunkNames rawExports
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
        Module.exports = resolveNamedDocExports namedDocChunks <$> rawExports,
        Module.imports = extractModuleImports lHsModule,
        Module.items =
          computeVisibility (resolveNamedDocExports namedDocChunks <$> rawExports)
            . extractItems referencedChunkNames
            $ lHsModule
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
extractModuleDocAndSince ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  (Doc.Doc, Maybe Since.Since)
extractModuleDocAndSince lHsModule =
  maybe (Doc.Empty, Nothing) GhcDoc.parseDoc (extractRawDocString lHsModule)

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
  Set.Set Text.Text ->
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  [Located.Located Item.Item]
extractItems referencedChunkNames lHsModule =
  let rawItems = Internal.runConvert $ extractItemsM referencedChunkNames lHsModule
      argNameMap = buildArgNameMap lHsModule
      patchedItems = patchArgumentNames argNameMap rawItems
      instanceHeadTypes = InstanceParents.extractInstanceHeadTypeNames lHsModule
      instanceClassNames = InstanceParents.extractInstanceClassNames lHsModule
      parentedItems = InstanceParents.associateInstanceParents instanceHeadTypes instanceClassNames patchedItems
      -- Parent-association passes: associate pragma/annotation items
      -- (warning, fixity, inline, specialise, type role) and family
      -- instance items with their target declarations by matching names.
      -- These run before merging so that 'mergeItemsByName' can remap
      -- already-established child 'parentKey's to the merged declaration key.
      warningLocations = WarningParents.extractWarningLocations lHsModule
      fixityLocations = FixityParents.extractFixityLocations lHsModule
      inlineLocations = InlineParents.extractInlineLocations lHsModule
      specialiseLocations = SpecialiseParents.extractSpecialiseLocations lHsModule
      roleLocations = RoleParents.extractRoleLocations lHsModule
      allPragmaLocations = Set.unions [warningLocations, fixityLocations, inlineLocations, specialiseLocations, roleLocations]
      warningParentedItems = ParentAssociation.associateParents allPragmaLocations warningLocations parentedItems
      fixityParentedItems = ParentAssociation.associateParents allPragmaLocations fixityLocations warningParentedItems
      inlineParentedItems = ParentAssociation.associateParents allPragmaLocations inlineLocations fixityParentedItems
      specialiseParentedItems = ParentAssociation.associateParents allPragmaLocations specialiseLocations inlineParentedItems
      roleParentedItems = ParentAssociation.associateParents allPragmaLocations roleLocations specialiseParentedItems
      familyInstanceNames = FamilyInstanceParents.extractFamilyInstanceNames lHsModule
      familyParentedItems = FamilyInstanceParents.associateFamilyInstanceParents familyInstanceNames roleParentedItems
      mergedItems = Merge.mergeItemsByName familyParentedItems
      -- Standalone kind signature merging runs after term-level
      -- merging so that type signatures and bindings are merged first.
      -- This merges standalone kind signatures into their corresponding
      -- declarations (see 'KindSigParents').
      kindSigParentedItems = KindSigParents.associateKindSigParents mergedItems
      -- COMPLETE pragma association runs after merging and uses inverted
      -- semantics: pattern synonyms are parented to the COMPLETE pragma
      -- (not the pragma to the patterns). This is because COMPLETE
      -- pragmas group patterns together rather than annotating a single
      -- declaration. It must run after merging because pattern synonyms
      -- are merged with their type signatures first.
      completeNames = CompleteParents.extractCompleteNames lHsModule
   in CompleteParents.associateCompleteParents completeNames kindSigParentedItems

-- | Whether an item kind is always visible regardless of the export list.
isAlwaysVisible :: ItemKind.ItemKind -> Bool
isAlwaysVisible k = case k of
  ItemKind.ClassInstance -> True
  ItemKind.StandaloneDeriving -> True
  ItemKind.DerivedInstance -> True
  ItemKind.Rule -> True
  ItemKind.Default -> True
  ItemKind.Annotation -> True
  ItemKind.Splice -> True
  _ -> False

-- | Compute visibility for each item based on the module's export list.
computeVisibility ::
  Maybe [Export.Export] ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
computeVisibility Nothing items = fmap setImplicit items
computeVisibility (Just exports) items =
  let exportedNames = extractExportedNames exports
      wildcardParentKeys = extractWildcardParentKeys exports items
   in fmap (classifyItem exportedNames wildcardParentKeys) items
  where
    classifyItem ::
      Set.Set Text.Text ->
      Set.Set ItemKey.ItemKey ->
      Located.Located Item.Item ->
      Located.Located Item.Item
    classifyItem exportedNames wildcardKeys li =
      let item = Located.value li
       in if isAlwaysVisible (Item.kind item)
            then setVisibility Visibility.Implicit li
            else case Item.parentKey item of
              Just pk
                | Set.member pk wildcardKeys ->
                    setVisibility Visibility.Exported li
              _ ->
                case Item.name item of
                  Just n
                    | Set.member (ItemName.unwrap n) exportedNames ->
                        setVisibility Visibility.Exported li
                  _ -> setVisibility Visibility.Unexported li

-- | When there is no export list, tag implicit items and leave everything
-- else as 'Exported' (the default set by 'mkItemM').
setImplicit :: Located.Located Item.Item -> Located.Located Item.Item
setImplicit li =
  let item = Located.value li
   in if isAlwaysVisible (Item.kind item)
        then setVisibility Visibility.Implicit li
        else li

-- | Set the visibility field on a located item.
setVisibility :: Visibility.Visibility -> Located.Located Item.Item -> Located.Located Item.Item
setVisibility v li =
  li {Located.value = (Located.value li) {Item.visibility = v}}

-- | Extract the set of names that are directly exported (including
-- explicitly named subordinates like @Foo(Bar, Baz)@).
extractExportedNames :: [Export.Export] -> Set.Set Text.Text
extractExportedNames = foldMap go
  where
    go :: Export.Export -> Set.Set Text.Text
    go (Export.Identifier ident) =
      let name = ExportName.name (ExportIdentifier.name ident)
          explicitSubs = case ExportIdentifier.subordinates ident of
            Nothing -> Set.empty
            Just (Subordinates.MkSubordinates _ explicit) ->
              Set.fromList $ fmap ExportName.name explicit
       in Set.insert name explicitSubs
    go _ = Set.empty

-- | Extract the set of parent item keys whose children are exported via
-- a @(..)@ wildcard.
extractWildcardParentKeys ::
  [Export.Export] ->
  [Located.Located Item.Item] ->
  Set.Set ItemKey.ItemKey
extractWildcardParentKeys exports items =
  let wildcardNames =
        Set.fromList
          [ ExportName.name (ExportIdentifier.name ident)
          | Export.Identifier ident <- exports,
            Just (Subordinates.MkSubordinates True _) <- [ExportIdentifier.subordinates ident]
          ]
      nameToKey =
        Map.fromList
          [ (ItemName.unwrap n, Item.key (Located.value li))
          | li <- items,
            Maybe.isNothing (Item.parentKey (Located.value li)),
            Just n <- [Item.name (Located.value li)]
          ]
   in Set.fromList
        . Maybe.mapMaybe (\n -> Map.lookup n nameToKey)
        $ Set.toList wildcardNames

-- | Build a map from function name to argument names extracted from
-- 'FunBind' patterns. Each function maps to a list of 'Maybe Text'
-- with one entry per argument position.
buildArgNameMap ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Map.Map ItemName.ItemName [Maybe Text.Text]
buildArgNameMap lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
   in Map.fromList
        [ (name, argNames)
        | lDecl <- decls,
          Syntax.ValD _ bind <- [SrcLoc.unLoc lDecl],
          let argNames = Names.extractBindArgNames bind,
          not (null argNames),
          Just name <- [Names.extractBindName bind]
        ]

-- | Patch argument names from function bindings into 'Argument' items.
--
-- For each 'Function' item whose name appears in the map, finds its
-- child 'Argument' items (by matching 'parentKey') and sets their
-- 'name' field from the corresponding position in the argument name
-- list.
patchArgumentNames ::
  Map.Map ItemName.ItemName [Maybe Text.Text] ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
patchArgumentNames argNameMap items =
  let -- Map from function key to arg names
      funcKeyToArgNames =
        Map.fromList
          [ (Item.key val, names)
          | locItem <- items,
            let val = Located.value locItem,
            Item.kind val == ItemKind.Function || Item.kind val == ItemKind.Operator,
            Just itemName <- [Item.name val],
            Just names <- [Map.lookup itemName argNameMap]
          ]
      -- Group argument items by parent key, preserving order
      argsByParent =
        Map.fromListWith
          (flip (<>))
          [ (pk, [locItem])
          | locItem <- items,
            let val = Located.value locItem,
            Item.kind val == ItemKind.Argument,
            Just pk <- [Item.parentKey val]
          ]
      -- Build update map: item key -> updated item
      updates =
        Map.fromList
          [ (Item.key (Located.value updated), updated)
          | (funcKey, names) <- Map.toList funcKeyToArgNames,
            Just argItems <- [Map.lookup funcKey argsByParent],
            updated <- zipWith setArgName (names <> repeat Nothing) argItems
          ]
   in fmap (\item -> Maybe.fromMaybe item $ Map.lookup (Item.key (Located.value item)) updates) items

-- | Set the name of an argument item if a name is provided.
setArgName :: Maybe Text.Text -> Located.Located Item.Item -> Located.Located Item.Item
setArgName mName locItem = case mName of
  Nothing -> locItem
  Just name ->
    locItem
      { Located.value =
          (Located.value locItem)
            { Item.name = Just $ ItemName.MkItemName name
            }
      }

-- | Extract items in the conversion monad.
extractItemsM ::
  Set.Set Text.Text ->
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Internal.ConvertM [Located.Located Item.Item]
extractItemsM referencedChunkNames lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
      declsWithDocs = GhcDoc.associateDocs referencedChunkNames decls
  concat <$> traverse (\(doc, docSince, lDecl) -> convertDeclWithDocMaybeM doc docSince lDecl) declsWithDocs

-- | Convert a declaration with documentation.
convertDeclWithDocMaybeM ::
  Doc.Doc ->
  Maybe Since.Since ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertDeclWithDocMaybeM doc docSince lDecl = case SrcLoc.unLoc lDecl of
  Syntax.TyClD _ tyClDecl -> convertTyClDeclWithDocM doc docSince lDecl tyClDecl
  Syntax.RuleD _ ruleDecls -> convertRuleDeclsM ruleDecls
  Syntax.DocD _ (Hs.DocCommentNamed name lNamedDoc) ->
    let chunkName = Just . ItemName.MkItemName . Text.pack $ '$' : name
        chunkDoc = GhcDoc.convertExportDoc lNamedDoc
     in Maybe.maybeToList <$> Internal.mkItemM (Annotation.getLocA lDecl) Nothing chunkName (Internal.appendDoc doc chunkDoc) docSince Nothing ItemKind.DocumentationChunk
  Syntax.DocD {} -> Maybe.maybeToList <$> convertDeclSimpleM lDecl
  Syntax.SigD _ sig -> convertSigDeclM doc docSince lDecl sig
  Syntax.KindSigD _ kindSig ->
    let sig = Just $ Names.extractKindSigSignature kindSig
     in Maybe.maybeToList <$> convertDeclWithDocM Nothing doc docSince (Just $ Names.extractStandaloneKindSigName kindSig) sig lDecl
  Syntax.InstD _ inst -> convertInstDeclWithDocM doc docSince lDecl inst
  Syntax.ForD _ foreignDecl ->
    let name = Just $ Names.extractForeignDeclName foreignDecl
        sig = Just $ Names.extractForeignDeclSignature foreignDecl
     in Maybe.maybeToList <$> convertDeclWithDocM Nothing doc docSince name sig lDecl
  Syntax.SpliceD _ spliceDecl ->
    let sig = Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ spliceDecl
     in Maybe.maybeToList <$> convertDeclWithDocM Nothing doc docSince Nothing sig lDecl
  Syntax.WarningD _ warnDecls -> convertWarnDeclsM warnDecls
  Syntax.RoleAnnotD _ roleAnnotDecl -> Maybe.maybeToList <$> convertRoleAnnotM roleAnnotDecl
  Syntax.DefD {} -> pure []
  Syntax.DerivD _ derivDecl ->
    let strategy = extractDerivStrategy $ Syntax.deriv_strategy derivDecl
     in Maybe.maybeToList <$> convertDeclWithDocM Nothing doc docSince (Names.extractDeclName lDecl) strategy lDecl
  _ -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc docSince (Names.extractDeclName lDecl) Nothing lDecl

-- | Convert a type/class declaration with documentation.
convertTyClDeclWithDocM ::
  Doc.Doc ->
  Maybe Since.Since ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Syntax.TyClDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertTyClDeclWithDocM doc docSince lDecl tyClDecl = case tyClDecl of
  Syntax.FamDecl _ famDecl -> case Syntax.fdInfo famDecl of
    Syntax.ClosedTypeFamily (Just eqns) -> do
      parentItem <- convertDeclWithDocM Nothing doc docSince (Names.extractTyClDeclName tyClDecl) Nothing lDecl
      let parentKey = fmap (Item.key . Located.value) parentItem
      eqnItems <- convertTyFamInstEqnsM parentKey eqns
      pure $ Maybe.maybeToList parentItem <> eqnItems
    _ -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc docSince (Names.extractTyClDeclName tyClDecl) Nothing lDecl
  Syntax.DataDecl _ _ _ _ dataDefn -> do
    parentItem <- convertDeclWithDocM Nothing doc docSince (Names.extractTyClDeclName tyClDecl) (Names.extractTyClDeclTyVars tyClDecl) lDecl
    let parentKey = fmap (Item.key . Located.value) parentItem
        parentType = Names.extractParentTypeText tyClDecl
    childItems <- convertDataDefnM parentKey parentType dataDefn
    pure $ Maybe.maybeToList parentItem <> childItems
  Syntax.ClassDecl {Syntax.tcdSigs = sigs, Syntax.tcdATs = ats, Syntax.tcdDocs = docs} -> do
    parentItem <- convertDeclWithDocM Nothing doc docSince (Names.extractTyClDeclName tyClDecl) (Names.extractTyClDeclTyVars tyClDecl) lDecl
    let parentKey = fmap (Item.key . Located.value) parentItem
    methodItems <- convertClassSigsWithDocsM parentKey sigs docs
    defaultSigItems <- convertDefaultSigsM methodItems sigs docs
    minimalItems <- convertMinimalSigsM parentKey sigs
    familyItems <- convertFamilyDeclsM parentKey ats
    pure $ Maybe.maybeToList parentItem <> methodItems <> defaultSigItems <> minimalItems <> familyItems
  Syntax.SynDecl {} -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc docSince (Names.extractTyClDeclName tyClDecl) (Names.extractSynDeclSignature tyClDecl) lDecl

-- | Convert an instance declaration with documentation.
convertInstDeclWithDocM ::
  Doc.Doc ->
  Maybe Since.Since ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Syntax.InstDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertInstDeclWithDocM doc docSince lDecl inst = case inst of
  Syntax.DataFamInstD _ dataFamInst -> do
    parentItem <- convertDeclWithDocM Nothing doc docSince (Names.extractInstDeclName inst) Nothing lDecl
    let parentKey = fmap (Item.key . Located.value) parentItem
        eqn = Syntax.dfid_eqn dataFamInst
        parentType =
          Just . Text.pack . Outputable.showSDocUnsafe $
            Outputable.ppr (Syntax.feqn_tycon eqn)
              Outputable.<+> Outputable.hsep (pprHsTypeArg <$> Syntax.feqn_pats eqn)
    childItems <- convertDataDefnM parentKey parentType (Syntax.feqn_rhs eqn)
    pure $ Maybe.maybeToList parentItem <> childItems
  _ -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc docSince (Names.extractInstDeclName inst) Nothing lDecl

-- | Convert a signature declaration.
convertSigDeclM ::
  Doc.Doc ->
  Maybe Since.Since ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Syntax.Sig Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertSigDeclM doc docSince lDecl sig = case sig of
  Syntax.TypeSig _ names _ ->
    let sigText = Names.extractSigSignature sig
        args = Names.extractSigArguments sig
     in fmap concat . Traversable.for names $ \lName -> do
          parentResult <-
            Internal.mkItemWithKeyM
              (Annotation.getLocA lName)
              Nothing
              (Just $ Internal.extractIdPName lName)
              doc
              docSince
              sigText
              (ItemKindFrom.functionOrOperator lName)
          case parentResult of
            Nothing -> pure []
            Just (parentItem, parentKey) -> do
              argItems <- convertArguments (Just parentKey) (Annotation.getLocA lName) args
              pure $ [parentItem] <> argItems
  Syntax.PatSynSig _ names _ ->
    let sigText = Names.extractSigSignature sig
        args = Names.extractSigArguments sig
     in fmap concat . Traversable.for names $ \lName -> do
          parentResult <-
            Internal.mkItemWithKeyM
              (Annotation.getLocA lName)
              Nothing
              (Just $ Internal.extractIdPName lName)
              doc
              docSince
              sigText
              ItemKind.PatternSynonym
          case parentResult of
            Nothing -> pure []
            Just (parentItem, parentKey) -> do
              argItems <- convertArguments (Just parentKey) (Annotation.getLocA lName) args
              pure $ [parentItem] <> argItems
  Syntax.FixSig _ (Syntax.FixitySig _ names (SyntaxBasic.Fixity prec dir)) ->
    let fixityDoc = Doc.Paragraph . Doc.String $ fixityDirectionToText dir <> Text.pack (" " <> show prec)
        combinedDoc = combineDoc doc fixityDoc
     in Maybe.catMaybes <$> traverse (convertFixityNameM combinedDoc) names
  Syntax.InlineSig _ lName inlinePragma ->
    let sigText = Just . inlineSpecToText $ Basic.inl_inline inlinePragma
     in Maybe.maybeToList <$> convertInlineNameM doc docSince sigText lName
  Syntax.SpecSig _ lName sigTypes _ ->
    let sigText = Just . Text.pack . Outputable.showSDocUnsafe $ Outputable.hsep (Outputable.punctuate (Outputable.text ",") (fmap Outputable.ppr sigTypes))
     in Maybe.maybeToList <$> convertSpecialiseNameM doc docSince sigText lName
  Syntax.SpecSigE _ _ lExpr _ -> convertSpecSigEM doc docSince lExpr
  Syntax.CompleteMatchSig _ names mTyCon ->
    let namesSig = Outputable.hsep (Outputable.punctuate (Outputable.text ",") (fmap Outputable.ppr names))
        sigText = Just . Text.pack . Outputable.showSDocUnsafe $ case mTyCon of
          Nothing -> namesSig
          Just tyCon -> namesSig Outputable.<+> Outputable.text "::" Outputable.<+> Outputable.ppr tyCon
     in Maybe.maybeToList <$> Internal.mkItemM (Annotation.getLocA lDecl) Nothing Nothing doc docSince sigText ItemKind.CompletePragma
  _ -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc docSince (Names.extractSigName sig) Nothing lDecl

-- | Convert extracted argument data into child Argument items.
convertArguments ::
  Maybe ItemKey.ItemKey ->
  SrcLoc.SrcSpan ->
  [(Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))] ->
  Internal.ConvertM [Located.Located Item.Item]
convertArguments parentKey srcSpan args =
  Maybe.catMaybes <$> traverse (convertOneArgument parentKey srcSpan) args

-- | Convert a single argument to an Argument item.
convertOneArgument ::
  Maybe ItemKey.ItemKey ->
  SrcLoc.SrcSpan ->
  (Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs)) ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertOneArgument parentKey srcSpan (sigText, mDoc) =
  let (argDoc, argSince) = maybe (Doc.Empty, Nothing) GhcDoc.convertLHsDoc mDoc
   in Internal.mkItemM srcSpan parentKey Nothing argDoc argSince (Just sigText) ItemKind.Argument

-- | Convert a single name from a fixity signature.
convertFixityNameM ::
  Doc.Doc ->
  Syntax.LIdP Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertFixityNameM fixityDoc lName =
  Internal.mkItemM (Annotation.getLocA lName) Nothing (Just $ Internal.extractIdPName lName) fixityDoc Nothing Nothing ItemKind.FixitySignature

-- | Convert a single name from an inline signature.
convertInlineNameM ::
  Doc.Doc ->
  Maybe Since.Since ->
  Maybe Text.Text ->
  Syntax.LIdP Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertInlineNameM doc docSince sig lName =
  Internal.mkItemM (Annotation.getLocA lName) Nothing (Just $ Internal.extractIdPName lName) doc docSince sig ItemKind.InlineSignature

-- | Convert a single name from a SPECIALIZE signature.
convertSpecialiseNameM ::
  Doc.Doc ->
  Maybe Since.Since ->
  Maybe Text.Text ->
  Syntax.LIdP Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertSpecialiseNameM doc docSince sig lName =
  Internal.mkItemM (Annotation.getLocA lName) Nothing (Just $ Internal.extractIdPName lName) doc docSince sig ItemKind.SpecialiseSignature

-- | Convert a SpecSigE expression to items.
convertSpecSigEM ::
  Doc.Doc ->
  Maybe Since.Since ->
  Syntax.LHsExpr Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertSpecSigEM doc docSince lExpr = case SrcLoc.unLoc lExpr of
  Syntax.ExprWithTySig _ body sigWcType ->
    let sigText = Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ sigWcType
     in case SrcLoc.unLoc body of
          Syntax.HsVar _ lName ->
            Maybe.maybeToList <$> convertSpecialiseNameM doc docSince sigText lName
          _ ->
            Maybe.maybeToList <$> Internal.mkItemM (Annotation.getLocA lExpr) Nothing Nothing doc docSince sigText ItemKind.SpecialiseSignature
  _ ->
    Maybe.maybeToList <$> Internal.mkItemM (Annotation.getLocA lExpr) Nothing Nothing doc docSince Nothing ItemKind.SpecialiseSignature

-- | Combine a user-written doc with a synthesized doc. If the user doc
-- is empty, just use the synthesized one; otherwise append both.
combineDoc :: Doc.Doc -> Doc.Doc -> Doc.Doc
combineDoc Doc.Empty synth = synth
combineDoc user synth = Doc.Append [user, synth]

-- | Convert a fixity direction to text.
fixityDirectionToText :: SyntaxBasic.FixityDirection -> Text.Text
fixityDirectionToText dir = case dir of
  SyntaxBasic.InfixL -> Text.pack "infixl"
  SyntaxBasic.InfixR -> Text.pack "infixr"
  SyntaxBasic.InfixN -> Text.pack "infix"

-- | Convert a GHC 'InlineSpec' to its pragma keyword text.
inlineSpecToText :: Basic.InlineSpec -> Text.Text
inlineSpecToText inlineSpec = case inlineSpec of
  Basic.Inline {} -> Text.pack "INLINE"
  Basic.Inlinable {} -> Text.pack "INLINABLE"
  Basic.NoInline {} -> Text.pack "NOINLINE"
  Basic.Opaque {} -> Text.pack "OPAQUE"
  Basic.NoUserInlinePrag -> Text.pack "INLINE"

-- | Convert a simple declaration without special handling.
convertDeclSimpleM ::
  Syntax.LHsDecl Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertDeclSimpleM = convertDeclWithDocM Nothing Doc.Empty Nothing Nothing Nothing

-- | Convert a declaration with documentation.
convertDeclWithDocM ::
  Maybe ItemKey.ItemKey ->
  Doc.Doc ->
  Maybe Since.Since ->
  Maybe ItemName.ItemName ->
  Maybe Text.Text ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertDeclWithDocM parentKey doc docSince itemName sig lDecl =
  let itemKind = ItemKindFrom.itemKindFromDecl $ SrcLoc.unLoc lDecl
   in Internal.mkItemM (Annotation.getLocA lDecl) parentKey itemName doc docSince sig itemKind

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
  let ruleDecl = SrcLoc.unLoc lRuleDecl
      name = Just . ItemName.MkItemName . Text.pack . FastString.unpackFS . SrcLoc.unLoc $ Syntax.rd_name ruleDecl
      sig =
        Just . Text.pack . Outputable.showSDocUnsafe $
          Outputable.ppr (Syntax.rd_bndrs ruleDecl)
            Outputable.<+> Outputable.ppr (Syntax.rd_lhs ruleDecl)
            Outputable.<+> Outputable.text "="
            Outputable.<+> Outputable.ppr (Syntax.rd_rhs ruleDecl)
   in Internal.mkItemM (Annotation.getLocA lRuleDecl) Nothing name Doc.Empty Nothing sig ItemKind.Rule

-- | Convert a role annotation declaration.
convertRoleAnnotM ::
  Syntax.RoleAnnotDecl Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertRoleAnnotM (Syntax.RoleAnnotDecl _ lName roles) =
  let sig = Just . Text.intercalate (Text.pack " ") $ fmap (roleToText . SrcLoc.unLoc) roles
   in Internal.mkItemM (Annotation.getLocA lName) Nothing (Just $ Internal.extractIdPName lName) Doc.Empty Nothing sig ItemKind.RoleAnnotation

-- | Convert a Maybe Role to its textual representation.
roleToText :: Maybe SyntaxBasic.Role -> Text.Text
roleToText r = case r of
  Nothing -> Text.pack "_"
  Just SyntaxBasic.Nominal -> Text.pack "nominal"
  Just SyntaxBasic.Representational -> Text.pack "representational"
  Just SyntaxBasic.Phantom -> Text.pack "phantom"

-- | Convert warning declarations.
convertWarnDeclsM ::
  Syntax.WarnDecls Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertWarnDeclsM (Syntax.Warnings _ warnDecls) =
  concat <$> traverse convertWarnDeclM warnDecls

-- | Convert a single warning declaration.
convertWarnDeclM ::
  Syntax.LWarnDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertWarnDeclM lWarnDecl = case SrcLoc.unLoc lWarnDecl of
  Syntax.Warning _ names warningTxt ->
    let warning = Internal.warningTxtToWarning warningTxt
     in Maybe.catMaybes <$> traverse (convertWarnNameM warning) names

-- | Convert a single name from a warning declaration.
convertWarnNameM ::
  Warning.Warning ->
  Syntax.LIdP Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertWarnNameM warning lName =
  let doc = Doc.Paragraph . Doc.String $ Warning.value warning
      sig = Just . Category.unwrap $ Warning.category warning
   in Internal.mkItemM (Annotation.getLocA lName) Nothing (Just $ Internal.extractIdPName lName) doc Nothing sig ItemKind.Warning

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
      sigsWithDocs = GhcDoc.associateDocs Set.empty allDecls
   in concat <$> traverse (\(doc, docSince, lDecl) -> convertClassDeclWithDocM parentKey doc docSince lDecl) sigsWithDocs
  where
    isClassOpSig :: Syntax.LSig Ghc.GhcPs -> Bool
    isClassOpSig lSig = case SrcLoc.unLoc lSig of
      Syntax.ClassOpSig _ False _ _ -> True
      _ -> False

-- | Convert a class body declaration with associated documentation.
convertClassDeclWithDocM ::
  Maybe ItemKey.ItemKey ->
  Doc.Doc ->
  Maybe Since.Since ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertClassDeclWithDocM parentKey doc docSince lDecl = case SrcLoc.unLoc lDecl of
  Syntax.SigD _ sig -> case sig of
    Syntax.ClassOpSig _ _ names _ ->
      let sigText = Names.extractSigSignature sig
          args = Names.extractSigArguments sig
       in fmap concat . Traversable.for names $ \lName -> do
            parentResult <-
              Internal.mkItemWithKeyM
                (Annotation.getLocA lName)
                parentKey
                (Just $ Internal.extractIdPName lName)
                doc
                docSince
                sigText
                ItemKind.ClassMethod
            case parentResult of
              Nothing -> pure []
              Just (methodItem, methodKey) -> do
                argItems <- convertArguments (Just methodKey) (Annotation.getLocA lName) args
                pure $ [methodItem] <> argItems
    _ -> pure []
  _ -> pure []

-- | Convert default method signatures within a class, parenting them to the
-- corresponding class method.
convertDefaultSigsM ::
  [Located.Located Item.Item] ->
  [Syntax.LSig Ghc.GhcPs] ->
  [Hs.LDocDecl Ghc.GhcPs] ->
  Internal.ConvertM [Located.Located Item.Item]
convertDefaultSigsM methodItems sigs docs =
  let nameToKey =
        Map.fromList
          [ (name, Item.key item)
          | Located.MkLocated _ item <- methodItems,
            Just name <- [Item.name item]
          ]
      defaultSigs = filter isDefaultSig sigs
      sigDecls = fmap (fmap (Syntax.SigD Hs.noExtField)) defaultSigs
      docDecls = fmap (fmap (Syntax.DocD Hs.noExtField)) docs
      allDecls = List.sortBy (\a b -> SrcLoc.leftmost_smallest (Annotation.getLocA a) (Annotation.getLocA b)) (sigDecls <> docDecls)
      sigsWithDocs = GhcDoc.associateDocs Set.empty allDecls
   in concat <$> traverse (\(doc, docSince, lDecl) -> convertDefaultSigDeclM nameToKey doc docSince lDecl) sigsWithDocs
  where
    isDefaultSig :: Syntax.LSig Ghc.GhcPs -> Bool
    isDefaultSig lSig = case SrcLoc.unLoc lSig of
      Syntax.ClassOpSig _ True _ _ -> True
      _ -> False

-- | Convert a single default method signature declaration with documentation.
convertDefaultSigDeclM ::
  Map.Map ItemName.ItemName ItemKey.ItemKey ->
  Doc.Doc ->
  Maybe Since.Since ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertDefaultSigDeclM nameToKey doc docSince lDecl = case SrcLoc.unLoc lDecl of
  Syntax.SigD _ (Syntax.ClassOpSig _ True names sigTy) ->
    let sig = Just . Text.pack . Outputable.showSDocUnsafe $ Outputable.ppr sigTy
     in Maybe.catMaybes <$> traverse (convertDefaultSigNameM nameToKey doc docSince sig) names
  _ -> pure []

-- | Convert a single name from a default method signature.
convertDefaultSigNameM ::
  Map.Map ItemName.ItemName ItemKey.ItemKey ->
  Doc.Doc ->
  Maybe Since.Since ->
  Maybe Text.Text ->
  Syntax.LIdP Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertDefaultSigNameM nameToKey doc docSince sig lName =
  let name = Internal.extractIdPName lName
      parentKey = Map.lookup name nameToKey
   in Internal.mkItemM (Annotation.getLocA lName) parentKey (Just name) doc docSince sig ItemKind.DefaultMethodSignature

-- | Convert MINIMAL pragma signatures inside a class.
convertMinimalSigsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.LSig Ghc.GhcPs] ->
  Internal.ConvertM [Located.Located Item.Item]
convertMinimalSigsM parentKey = fmap Maybe.catMaybes . traverse (convertMinimalSigM parentKey)

-- | Convert a single MINIMAL pragma signature.
convertMinimalSigM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LSig Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertMinimalSigM parentKey lSig = case SrcLoc.unLoc lSig of
  Syntax.MinimalSig _ lBooleanFormula ->
    let sig = Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ SrcLoc.unLoc lBooleanFormula
     in Internal.mkItemM (Annotation.getLocA lSig) parentKey Nothing Doc.Empty Nothing sig ItemKind.MinimalPragma
  _ -> pure Nothing

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
   in Internal.mkItemM (Annotation.getLocA lEqn) parentKey Nothing Doc.Empty Nothing sig ItemKind.TypeFamilyInstance

-- | Pretty-print a type family instance equation.
extractTyFamInstEqnSig :: Syntax.TyFamInstEqn Ghc.GhcPs -> Outputable.SDoc
extractTyFamInstEqnSig eqn =
  Outputable.ppr (Syntax.feqn_tycon eqn)
    Outputable.<+> Outputable.hsep (pprHsTypeArg <$> Syntax.feqn_pats eqn)
    Outputable.<+> Outputable.text "="
    Outputable.<+> Outputable.ppr (Syntax.feqn_rhs eqn)

-- | Pretty-print a type argument, stripping the 'HsArg' wrapper.
pprHsTypeArg :: Syntax.LHsTypeArg Ghc.GhcPs -> Outputable.SDoc
pprHsTypeArg (Syntax.HsValArg _ ty) = Outputable.ppr ty
pprHsTypeArg (Syntax.HsTypeArg _ ki) = Outputable.text "@" Outputable.<> Outputable.ppr ki
pprHsTypeArg (Syntax.HsArgPar _) = Outputable.empty

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
      strategy = extractDerivStrategy $ Syntax.deriv_clause_strategy clause
      derivClauseTys = SrcLoc.unLoc $ Syntax.deriv_clause_tys clause
  convertDerivClauseTysM parentKey strategy derivClauseTys

-- | Convert deriving clause types.
convertDerivClauseTysM ::
  Maybe ItemKey.ItemKey ->
  Maybe Text.Text ->
  Syntax.DerivClauseTys Ghc.GhcPs ->
  Internal.ConvertM [Located.Located Item.Item]
convertDerivClauseTysM parentKey strategy dct = case dct of
  Syntax.DctSingle _ lSigTy -> Maybe.maybeToList <$> convertDerivedTypeM parentKey strategy lSigTy
  Syntax.DctMulti _ lSigTys -> Maybe.catMaybes <$> traverse (convertDerivedTypeM parentKey strategy) lSigTys

-- | Convert a derived type to an item.
convertDerivedTypeM ::
  Maybe ItemKey.ItemKey ->
  Maybe Text.Text ->
  Syntax.LHsSigType Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertDerivedTypeM parentKey strategy lSigTy =
  let (doc, docSince) = extractDerivedTypeDocAndSince lSigTy
   in Internal.mkItemM (Annotation.getLocA lSigTy) parentKey (extractDerivedTypeName lSigTy) doc docSince strategy ItemKind.DerivedInstance

-- | Extract name from a derived type.
extractDerivedTypeName :: Syntax.LHsSigType Ghc.GhcPs -> Maybe ItemName.ItemName
extractDerivedTypeName lSigTy =
  let sigTy = SrcLoc.unLoc lSigTy
      bodyTy = SrcLoc.unLoc $ Syntax.sig_body sigTy
      ty = case bodyTy of
        Syntax.HsDocTy _ lTy _ -> SrcLoc.unLoc lTy
        _ -> bodyTy
   in Just . ItemName.MkItemName . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ ty

-- | Extract documentation and @since from a derived type.
extractDerivedTypeDocAndSince :: Syntax.LHsSigType Ghc.GhcPs -> (Doc.Doc, Maybe Since.Since)
extractDerivedTypeDocAndSince lSigTy =
  let sigTy = SrcLoc.unLoc lSigTy
      bodyTy = SrcLoc.unLoc $ Syntax.sig_body sigTy
   in case bodyTy of
        Syntax.HsDocTy _ _ lDoc -> GhcDoc.convertLHsDoc lDoc
        _ -> (Doc.Empty, Nothing)

-- | Extract deriving strategy text from a deriving clause.
extractDerivStrategy ::
  Maybe (Syntax.LDerivStrategy Ghc.GhcPs) ->
  Maybe Text.Text
extractDerivStrategy = fmap (Text.pack . Outputable.showSDocUnsafe . Outputable.ppr . SrcLoc.unLoc)

-- | Extract named documentation chunks from module declarations.
extractNamedDocChunks ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Map.Map Text.Text Doc.Doc
extractNamedDocChunks lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
   in Map.fromList $ Maybe.mapMaybe extractNamedDocChunk decls

-- | Extract a named doc chunk from a declaration, if applicable.
extractNamedDocChunk ::
  Syntax.LHsDecl Ghc.GhcPs ->
  Maybe (Text.Text, Doc.Doc)
extractNamedDocChunk lDecl = case SrcLoc.unLoc lDecl of
  Syntax.DocD _ (Hs.DocCommentNamed name lDoc) ->
    Just (Text.pack name, GhcDoc.convertExportDoc lDoc)
  _ -> Nothing

-- | Extract the set of named chunk names referenced in an export list.
extractReferencedChunkNames :: Maybe [Export.Export] -> Set.Set Text.Text
extractReferencedChunkNames Nothing = Set.empty
extractReferencedChunkNames (Just exports) =
  Set.fromList [name | Export.DocNamed name <- exports]

-- | Resolve named documentation chunk references in an export list.
resolveNamedDocExports ::
  Map.Map Text.Text Doc.Doc ->
  [Export.Export] ->
  [Export.Export]
resolveNamedDocExports namedChunks = fmap (resolveNamedDocExport namedChunks)

-- | Resolve a single named documentation chunk reference.
resolveNamedDocExport ::
  Map.Map Text.Text Doc.Doc ->
  Export.Export ->
  Export.Export
resolveNamedDocExport namedChunks export = case export of
  Export.DocNamed name ->
    maybe export Export.Doc (Map.lookup name namedChunks)
  _ -> export
