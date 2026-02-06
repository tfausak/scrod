{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Convert.FromGhc where

import qualified Control.Monad.Trans.State.Strict as State
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
import qualified GHC.Hs.ImpExp as ImpExp
import qualified GHC.LanguageExtensions.Type as GhcExtension
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Types.Name.Reader as Reader
import qualified GHC.Types.SourceText as SourceText
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Unit.Module.Warnings as Warnings
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Syntax
import qualified Numeric.Natural as Natural
import qualified PackageInfo_scrod as PackageInfo
import qualified Scrod.Convert.FromHaddock as FromHaddock
import qualified Scrod.Core.Category as Category
import qualified Scrod.Core.Column as Column
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Export as Export
import qualified Scrod.Core.ExportIdentifier as ExportIdentifier
import qualified Scrod.Core.ExportName as ExportName
import qualified Scrod.Core.ExportNameKind as ExportNameKind
import qualified Scrod.Core.Extension as Extension
import qualified Scrod.Core.Header as Header
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Language as Language
import qualified Scrod.Core.Level as Level
import qualified Scrod.Core.Line as Line
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.Location as Location
import qualified Scrod.Core.Module as Module
import qualified Scrod.Core.ModuleName as ModuleName
import qualified Scrod.Core.Section as Section
import qualified Scrod.Core.Since as Since
import qualified Scrod.Core.Subordinates as Subordinates
import qualified Scrod.Core.Version as Version
import qualified Scrod.Core.Warning as Warning
import qualified Scrod.Ghc.OnOff as OnOff
import qualified Scrod.Spec as Spec

-- | State for tracking item keys during conversion.
newtype ConversionState = MkConversionState
  { nextKey :: Natural.Natural
  }

-- | Initial conversion state.
initialState :: ConversionState
initialState = MkConversionState {nextKey = 0}

-- | Allocate a new key from the state.
allocateKey :: ConversionState -> (ItemKey.ItemKey, ConversionState)
allocateKey s =
  let k = nextKey s
   in (ItemKey.MkItemKey k, s {nextKey = k + 1})

-- | Monad for item conversion with auto-incrementing keys.
type ConvertM a = State.State ConversionState a

-- | Allocate a new unique key.
allocateKeyM :: ConvertM ItemKey.ItemKey
allocateKeyM = State.state allocateKey

-- | Run the conversion monad and extract the result.
runConvert :: ConvertM a -> a
runConvert = flip State.evalState initialState

-- | Convert a parsed GHC module to the internal Module type.
fromGhc ::
  ( (Maybe Session.Language, [DynFlags.OnOff GhcExtension.Extension]),
    SrcLoc.Located (Hs.HsModule Ghc.GhcPs)
  ) ->
  Either String Module.Module
fromGhc ((language, extensions), lHsModule) = do
  version <- maybe (Left "invalid version") Right $ versionFromBase PackageInfo.version
  Right
    Module.MkModule
      { Module.version = version,
        Module.language = languageFromGhc <$> language,
        Module.extensions = extensionsToMap extensions,
        Module.documentation = extractModuleDocumentation lHsModule,
        Module.since = extractModuleSince lHsModule,
        Module.name = extractModuleName lHsModule,
        Module.warning = extractModuleWarning lHsModule,
        Module.exports = extractModuleExports lHsModule,
        Module.items = extractItems lHsModule
      }

-- | Convert base version to our Version type.
versionFromBase :: Data.Version.Version -> Maybe Version.Version
versionFromBase v = case Data.Version.versionBranch v of
  [] -> Nothing
  x : xs -> (Just . Version.MkVersion) . fmap fromIntegral $ x NonEmpty.:| xs

-- | Convert GHC language to our Language type.
languageFromGhc :: Session.Language -> Language.Language
languageFromGhc lang =
  Language.MkLanguage . Text.pack $ case lang of
    Session.Haskell98 -> "Haskell98"
    Session.Haskell2010 -> "Haskell2010"
    Session.GHC2021 -> "GHC2021"
    Session.GHC2024 -> "GHC2024"

-- | Convert GHC extension to our Extension type.
extensionFromGhc :: GhcExtension.Extension -> Extension.Extension
extensionFromGhc =
  Extension.MkExtension
    . Text.pack
    . Outputable.showSDocUnsafe
    . Outputable.ppr

-- | Convert list of OnOff extensions to a Map.
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
      moduleName = moduleNameFromGhc $ SrcLoc.unLoc lModuleName
  locatedFromGhc $ SrcLoc.L srcSpan moduleName

-- | Convert GHC module name to our ModuleName type.
moduleNameFromGhc :: Syntax.ModuleName -> ModuleName.ModuleName
moduleNameFromGhc =
  ModuleName.MkModuleName
    . Text.pack
    . Syntax.moduleNameString

-- | Convert GHC Located to our Located type.
locatedFromGhc :: SrcLoc.Located a -> Maybe (Located.Located a)
locatedFromGhc (SrcLoc.L srcSpan a) = do
  location <- locationFromSrcSpan srcSpan
  Just
    Located.MkLocated
      { Located.location = location,
        Located.value = a
      }

-- | Convert SrcSpan to our Location type.
locationFromSrcSpan :: SrcLoc.SrcSpan -> Maybe Location.Location
locationFromSrcSpan srcSpan = case srcSpan of
  SrcLoc.RealSrcSpan realSrcSpan _ ->
    Just
      Location.MkLocation
        { Location.line = Line.MkLine . fromIntegral $ SrcLoc.srcSpanStartLine realSrcSpan,
          Location.column = Column.MkColumn . fromIntegral $ SrcLoc.srcSpanStartCol realSrcSpan
        }
  SrcLoc.UnhelpfulSpan _ -> Nothing

-- | Extract module documentation from the parsed module.
extractModuleDocumentation ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Doc.Doc
extractModuleDocumentation =
  maybe Doc.Empty parseDoc
    . extractRawDocString

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

-- | Extract @since information from module documentation.
-- Note: Full @since extraction requires parsing Haddock metadata which is
-- not fully supported in all haddock-library versions. Returns Nothing for now.
extractModuleSince ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe Since.Since
extractModuleSince _ = Nothing

-- | Extract module deprecation warning.
extractModuleWarning ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe Warning.Warning
extractModuleWarning lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
      xModulePs = Syntax.hsmodExt hsModule
  lWarningTxt <- Hs.hsmodDeprecMessage xModulePs
  let warningTxt = SrcLoc.unLoc lWarningTxt
  Just $ warningTxtToWarning warningTxt

-- | Convert GHC WarningTxt to our Warning type.
warningTxtToWarning :: Warnings.WarningTxt Ghc.GhcPs -> Warning.Warning
warningTxtToWarning warningTxt =
  Warning.MkWarning
    { Warning.category = categoryFromGhc $ Warnings.warningTxtCategory warningTxt,
      Warning.value =
        Text.intercalate (Text.singleton '\n')
          . fmap extractMessage
          $ Warnings.warningTxtMessage warningTxt
    }

-- | Convert GHC WarningCategory to our Category type.
categoryFromGhc :: Warnings.WarningCategory -> Category.Category
categoryFromGhc =
  Category.MkCategory
    . Text.pack
    . Outputable.showSDocUnsafe
    . Outputable.ppr

-- | Extract message text from a located doc string.
extractMessage ::
  SrcLoc.GenLocated l (HsDoc.WithHsDocIdentifiers SourceText.StringLiteral Ghc.GhcPs) ->
  Text.Text
extractMessage =
  Text.pack
    . FastString.unpackFS
    . SourceText.sl_fs
    . HsDoc.hsDocString
    . SrcLoc.unLoc

-- | Extract module export list.
extractModuleExports ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe [Export.Export]
extractModuleExports lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
  lExports <- Syntax.hsmodExports hsModule
  let exports = SrcLoc.unLoc lExports
  Just $ fmap convertIE exports

-- | Convert an IE (import/export) entry to our Export type.
convertIE ::
  SrcLoc.GenLocated l (Syntax.IE Ghc.GhcPs) ->
  Export.Export
convertIE lIe = case SrcLoc.unLoc lIe of
  Syntax.IEVar mLWarning lName mDoc ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name = convertWrappedName lName,
          ExportIdentifier.subordinates = Nothing,
          ExportIdentifier.warning = convertExportWarning mLWarning,
          ExportIdentifier.doc = convertExportDoc <$> mDoc
        }
  Syntax.IEThingAbs mLWarning lName mDoc ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name = convertWrappedName lName,
          ExportIdentifier.subordinates = Nothing,
          ExportIdentifier.warning = convertExportWarning mLWarning,
          ExportIdentifier.doc = convertExportDoc <$> mDoc
        }
  Syntax.IEThingAll (mLWarning, _) lName mDoc ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name = convertWrappedName lName,
          ExportIdentifier.subordinates =
            Just
              Subordinates.MkSubordinates
                { Subordinates.wildcard = True,
                  Subordinates.explicit = []
                },
          ExportIdentifier.warning = convertExportWarning mLWarning,
          ExportIdentifier.doc = convertExportDoc <$> mDoc
        }
  Syntax.IEThingWith (mLWarning, _) lName wildcard children mDoc ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name = convertWrappedName lName,
          ExportIdentifier.subordinates =
            Just
              Subordinates.MkSubordinates
                { Subordinates.wildcard = hasWildcard wildcard,
                  Subordinates.explicit = fmap convertWrappedName children
                },
          ExportIdentifier.warning = convertExportWarning mLWarning,
          ExportIdentifier.doc = convertExportDoc <$> mDoc
        }
  Syntax.IEModuleContents (mLWarning, _) lModName ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name =
            ExportName.MkExportName
              { ExportName.kind = Just ExportNameKind.Module,
                ExportName.name = ModuleName.unwrap . moduleNameFromGhc $ SrcLoc.unLoc lModName
              },
          ExportIdentifier.subordinates = Nothing,
          ExportIdentifier.warning = convertExportWarning mLWarning,
          ExportIdentifier.doc = Nothing
        }
  Syntax.IEGroup _ level lDoc ->
    Export.Group
      Section.MkSection
        { Section.header =
            Header.MkHeader
              { Header.level = levelFromInt level,
                Header.title = convertLHsDoc lDoc
              }
        }
  Syntax.IEDoc _ lDoc ->
    Export.Doc $ convertLHsDoc lDoc
  Syntax.IEDocNamed _ name ->
    Export.DocNamed $ Text.pack name

-- | Check if an IE wildcard is present.
hasWildcard :: ImpExp.IEWildcard -> Bool
hasWildcard wildcard = case wildcard of
  ImpExp.NoIEWildcard -> False
  ImpExp.IEWildcard _ -> True

-- | Convert export warning.
convertExportWarning ::
  Maybe (SrcLoc.GenLocated l (Warnings.WarningTxt Ghc.GhcPs)) ->
  Maybe Warning.Warning
convertExportWarning = fmap (warningTxtToWarning . SrcLoc.unLoc)

-- | Convert a wrapped name to our ExportName type.
convertWrappedName ::
  SrcLoc.GenLocated l (ImpExp.IEWrappedName Ghc.GhcPs) ->
  ExportName.ExportName
convertWrappedName lWrapped = case SrcLoc.unLoc lWrapped of
  ImpExp.IEName _ lId ->
    ExportName.MkExportName
      { ExportName.kind = Nothing,
        ExportName.name = extractRdrName lId
      }
  ImpExp.IEPattern _ lId ->
    ExportName.MkExportName
      { ExportName.kind = Just ExportNameKind.Pattern,
        ExportName.name = extractRdrName lId
      }
  ImpExp.IEType _ lId ->
    ExportName.MkExportName
      { ExportName.kind = Just ExportNameKind.Type,
        ExportName.name = extractRdrName lId
      }
  ImpExp.IEDefault _ lId ->
    ExportName.MkExportName
      { ExportName.kind = Nothing,
        ExportName.name = extractRdrName lId
      }
  ImpExp.IEData _ lId ->
    ExportName.MkExportName
      { ExportName.kind = Nothing,
        ExportName.name = extractRdrName lId
      }

-- | Extract name from RdrName.
extractRdrName :: SrcLoc.GenLocated l Reader.RdrName -> Text.Text
extractRdrName =
  Text.pack
    . Outputable.showSDocUnsafe
    . Outputable.ppr
    . SrcLoc.unLoc

-- | Convert an int to a Level.
levelFromInt :: Int -> Level.Level
levelFromInt n = case n of
  1 -> Level.One
  2 -> Level.Two
  3 -> Level.Three
  4 -> Level.Four
  5 -> Level.Five
  6 -> Level.Six
  _ -> Level.One

-- | Convert export documentation.
convertExportDoc ::
  SrcLoc.GenLocated l (HsDoc.WithHsDocIdentifiers DocString.HsDocString Ghc.GhcPs) ->
  Doc.Doc
convertExportDoc lDoc =
  let hsDoc = SrcLoc.unLoc lDoc
      hsDocString = HsDoc.hsDocString hsDoc
      rendered = DocString.renderHsDocString hsDocString
   in parseDoc rendered

-- | Convert a located HsDoc to our Doc type.
convertLHsDoc ::
  SrcLoc.GenLocated l (HsDoc.WithHsDocIdentifiers DocString.HsDocString Ghc.GhcPs) ->
  Doc.Doc
convertLHsDoc = convertExportDoc

-- | Parse documentation string to our Doc type.
parseDoc :: String -> Doc.Doc
parseDoc input =
  let metaDoc :: Haddock.MetaDoc m Haddock.Identifier
      metaDoc = Haddock.parseParas Nothing input
      haddockDoc :: Haddock.DocH m Haddock.Identifier
      haddockDoc = Haddock._doc metaDoc
   in FromHaddock.fromHaddock haddockDoc

-- | Extract items from the module.
extractItems ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  [Located.Located Item.Item]
extractItems lHsModule =
  let rawItems = runConvert $ extractItemsM lHsModule
   in mergeItemsByName rawItems

-- | Extract items in the conversion monad.
extractItemsM ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  ConvertM [Located.Located Item.Item]
extractItemsM lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
      declsWithDocs = associateDocs decls
  concat <$> traverse (uncurry convertDeclWithDocMaybeM) declsWithDocs

-- | Associate documentation comments with their target declarations.
associateDocs ::
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associateDocs decls =
  let withNextDocs = associateNextDocs decls
      withAllDocs = associatePrevDocs withNextDocs
   in withAllDocs

-- | Associate DocCommentNext with the following declaration.
associateNextDocs ::
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associateNextDocs = associateNextDocsLoop Doc.Empty

-- | Recursive helper for associating next-doc comments.
associateNextDocsLoop ::
  Doc.Doc ->
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associateNextDocsLoop _ [] = []
associateNextDocsLoop pendingDoc (lDecl : rest) = case SrcLoc.unLoc lDecl of
  Syntax.DocD _ (Hs.DocCommentNext lDoc) ->
    let newDoc = appendDoc pendingDoc $ convertLHsDoc lDoc
     in associateNextDocsLoop newDoc rest
  Syntax.DocD _ (Hs.DocCommentPrev _) ->
    (Doc.Empty, lDecl) : associateNextDocsLoop Doc.Empty rest
  _ ->
    (pendingDoc, lDecl) : associateNextDocsLoop Doc.Empty rest

-- | Associate DocCommentPrev with the preceding declaration.
associatePrevDocs ::
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associatePrevDocs = reverse . associatePrevDocsLoop . reverse

-- | Recursive helper for associating prev-doc comments.
associatePrevDocsLoop ::
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associatePrevDocsLoop [] = []
associatePrevDocsLoop ((doc, lDecl) : rest) = case SrcLoc.unLoc lDecl of
  Syntax.DocD _ (Hs.DocCommentPrev lDoc) ->
    let prevDoc = convertLHsDoc lDoc
     in applyPrevDoc prevDoc $ associatePrevDocsLoop rest
  _ ->
    (doc, lDecl) : associatePrevDocsLoop rest

-- | Apply a prev-doc comment to the nearest preceding non-doc declaration.
applyPrevDoc ::
  Doc.Doc ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
applyPrevDoc _ [] = []
applyPrevDoc prevDoc ((existingDoc, lDecl) : rest) = case SrcLoc.unLoc lDecl of
  Syntax.DocD {} -> (existingDoc, lDecl) : applyPrevDoc prevDoc rest
  _ -> (appendDoc existingDoc prevDoc, lDecl) : rest

-- | Append two Doc values.
appendDoc :: Doc.Doc -> Doc.Doc -> Doc.Doc
appendDoc Doc.Empty d = d
appendDoc d Doc.Empty = d
appendDoc d1 d2 = Doc.Append d1 d2

-- | Convert a declaration with documentation.
convertDeclWithDocMaybeM ::
  Doc.Doc ->
  Syntax.LHsDecl Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertDeclWithDocMaybeM doc lDecl = case SrcLoc.unLoc lDecl of
  Syntax.TyClD _ tyClDecl -> convertTyClDeclWithDocM doc lDecl tyClDecl
  Syntax.RuleD _ ruleDecls -> convertRuleDeclsM ruleDecls
  Syntax.DocD {} -> Maybe.maybeToList <$> convertDeclSimpleM lDecl
  Syntax.SigD _ sig -> convertSigDeclM doc lDecl sig
  Syntax.KindSigD _ kindSig ->
    let sig = Just $ extractKindSigSignature kindSig
     in Maybe.maybeToList <$> convertDeclWithDocM Nothing doc (Just $ extractStandaloneKindSigName kindSig) sig lDecl
  _ -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc (extractDeclName lDecl) Nothing lDecl

-- | Convert a type/class declaration with documentation.
convertTyClDeclWithDocM ::
  Doc.Doc ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Syntax.TyClDecl Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertTyClDeclWithDocM doc lDecl tyClDecl = case tyClDecl of
  Syntax.DataDecl _ _ _ _ dataDefn -> do
    parentItem <- convertDeclWithDocM Nothing doc (extractTyClDeclName tyClDecl) Nothing lDecl
    let parentKey = fmap (Item.key . Located.value) parentItem
    childItems <- convertDataDefnM parentKey dataDefn
    pure $ Maybe.maybeToList parentItem <> childItems
  Syntax.ClassDecl {Syntax.tcdSigs = sigs, Syntax.tcdATs = ats} -> do
    parentItem <- convertDeclWithDocM Nothing doc (extractTyClDeclName tyClDecl) Nothing lDecl
    let parentKey = fmap (Item.key . Located.value) parentItem
    methodItems <- convertClassSigsM parentKey sigs
    familyItems <- convertFamilyDeclsM parentKey ats
    pure $ Maybe.maybeToList parentItem <> methodItems <> familyItems
  _ -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc (extractTyClDeclName tyClDecl) Nothing lDecl

-- | Convert a signature declaration.
convertSigDeclM ::
  Doc.Doc ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Syntax.Sig Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertSigDeclM doc lDecl sig = case sig of
  Syntax.TypeSig _ names _ ->
    let sigText = extractSigSignature sig
     in Maybe.catMaybes <$> traverse (convertSigNameM doc sigText) names
  Syntax.PatSynSig _ names _ ->
    let sigText = extractSigSignature sig
     in Maybe.catMaybes <$> traverse (convertSigNameM doc sigText) names
  _ -> Maybe.maybeToList <$> convertDeclWithDocM Nothing doc (extractSigName sig) Nothing lDecl

-- | Convert a single name from a signature.
convertSigNameM ::
  Doc.Doc ->
  Maybe Text.Text ->
  Syntax.LIdP Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertSigNameM doc sig lName =
  mkItemM (Annotation.getLocA lName) Nothing (Just $ extractIdPName lName) doc sig ItemKind.Function

-- | Create an Item from a source span with the given properties.
mkItemM ::
  SrcLoc.SrcSpan ->
  Maybe ItemKey.ItemKey ->
  Maybe ItemName.ItemName ->
  Doc.Doc ->
  Maybe Text.Text ->
  ItemKind.ItemKind ->
  ConvertM (Maybe (Located.Located Item.Item))
mkItemM srcSpan parentKey itemName doc sig itemKind =
  fmap fst <$> mkItemWithKeyM srcSpan parentKey itemName doc sig itemKind

-- | Create an Item and return both the item and its allocated key.
mkItemWithKeyM ::
  SrcLoc.SrcSpan ->
  Maybe ItemKey.ItemKey ->
  Maybe ItemName.ItemName ->
  Doc.Doc ->
  Maybe Text.Text ->
  ItemKind.ItemKind ->
  ConvertM (Maybe (Located.Located Item.Item, ItemKey.ItemKey))
mkItemWithKeyM srcSpan parentKey itemName doc sig itemKind =
  case locationFromSrcSpan srcSpan of
    Nothing -> pure Nothing
    Just location -> do
      key <- allocateKeyM
      pure $
        Just
          ( Located.MkLocated
              { Located.location = location,
                Located.value =
                  Item.MkItem
                    { Item.key = key,
                      Item.kind = itemKind,
                      Item.parentKey = parentKey,
                      Item.name = itemName,
                      Item.documentation = doc,
                      Item.signature = sig
                    }
              },
            key
          )

-- | Convert a simple declaration without special handling.
convertDeclSimpleM ::
  Syntax.LHsDecl Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertDeclSimpleM = convertDeclWithDocM Nothing Doc.Empty Nothing Nothing

-- | Convert a declaration with documentation.
convertDeclWithDocM ::
  Maybe ItemKey.ItemKey ->
  Doc.Doc ->
  Maybe ItemName.ItemName ->
  Maybe Text.Text ->
  Syntax.LHsDecl Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertDeclWithDocM parentKey doc itemName sig lDecl =
  let itemKind = itemKindFromDecl $ SrcLoc.unLoc lDecl
   in mkItemM (Annotation.getLocA lDecl) parentKey itemName doc sig itemKind

-- | Determine the ItemKind from a declaration.
itemKindFromDecl :: Syntax.HsDecl Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromDecl decl = case decl of
  Syntax.TyClD _ tyClDecl -> itemKindFromTyClDecl tyClDecl
  Syntax.ValD _ bind -> itemKindFromBind bind
  Syntax.SigD _ sig -> itemKindFromSig sig
  Syntax.InstD _ inst -> itemKindFromInstDecl inst
  Syntax.KindSigD {} -> ItemKind.StandaloneKindSig
  Syntax.DefD {} -> ItemKind.Default
  Syntax.ForD _ foreignDecl -> itemKindFromForeignDecl foreignDecl
  Syntax.WarningD {} -> ItemKind.Function -- Treat as function for now
  Syntax.AnnD {} -> ItemKind.Annotation
  Syntax.RuleD {} -> ItemKind.Rule
  Syntax.SpliceD {} -> ItemKind.Function -- TH splice
  Syntax.DocD {} -> ItemKind.Function -- Doc comment
  Syntax.RoleAnnotD {} -> ItemKind.Function -- Role annotation
  Syntax.DerivD {} -> ItemKind.StandaloneDeriving

-- | Determine ItemKind from a type/class declaration.
itemKindFromTyClDecl :: Syntax.TyClDecl Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromTyClDecl tyClDecl = case tyClDecl of
  Syntax.FamDecl _ famDecl -> itemKindFromFamilyDecl famDecl
  Syntax.SynDecl {} -> ItemKind.TypeSynonym
  Syntax.DataDecl _ _ _ _ dataDefn -> itemKindFromDataDefn dataDefn
  Syntax.ClassDecl {} -> ItemKind.Class

-- | Determine ItemKind from a data definition.
itemKindFromDataDefn :: Syntax.HsDataDefn Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromDataDefn dataDefn = case Syntax.dd_cons dataDefn of
  Syntax.NewTypeCon {} -> ItemKind.Newtype
  Syntax.DataTypeCons isTypeData _ ->
    if isTypeData
      then ItemKind.TypeData
      else ItemKind.DataType

-- | Determine ItemKind from a family declaration.
itemKindFromFamilyDecl :: Syntax.FamilyDecl Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromFamilyDecl famDecl = case Syntax.fdInfo famDecl of
  Syntax.DataFamily -> ItemKind.DataFamily
  Syntax.OpenTypeFamily -> ItemKind.OpenTypeFamily
  Syntax.ClosedTypeFamily {} -> ItemKind.ClosedTypeFamily

-- | Determine ItemKind from a binding.
itemKindFromBind :: Syntax.HsBindLR Ghc.GhcPs Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromBind bind = case bind of
  Syntax.FunBind {} -> ItemKind.Function
  Syntax.PatBind {} -> ItemKind.PatternBinding
  Syntax.VarBind {} -> ItemKind.Function
  Syntax.PatSynBind {} -> ItemKind.PatternSynonym

-- | Determine ItemKind from a signature.
itemKindFromSig :: Syntax.Sig Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromSig sig = case sig of
  Syntax.TypeSig {} -> ItemKind.Function
  Syntax.PatSynSig {} -> ItemKind.PatternSynonym
  Syntax.ClassOpSig {} -> ItemKind.ClassMethod
  Syntax.FixSig {} -> ItemKind.FixitySignature
  Syntax.InlineSig {} -> ItemKind.InlineSignature
  Syntax.SpecSig {} -> ItemKind.SpecialiseSignature
  _ -> ItemKind.Function

-- | Determine ItemKind from an instance declaration.
itemKindFromInstDecl :: Syntax.InstDecl Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromInstDecl inst = case inst of
  Syntax.ClsInstD {} -> ItemKind.ClassInstance
  Syntax.DataFamInstD {} -> ItemKind.DataFamilyInstance
  Syntax.TyFamInstD {} -> ItemKind.TypeFamilyInstance

-- | Determine ItemKind from a foreign declaration.
itemKindFromForeignDecl :: Syntax.ForeignDecl Ghc.GhcPs -> ItemKind.ItemKind
itemKindFromForeignDecl foreignDecl = case foreignDecl of
  Syntax.ForeignImport {} -> ItemKind.ForeignImport
  Syntax.ForeignExport {} -> ItemKind.ForeignExport

-- | Convert rule declarations.
convertRuleDeclsM ::
  Syntax.RuleDecls Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertRuleDeclsM (Syntax.HsRules _ rules) = Maybe.catMaybes <$> traverse convertRuleDeclM rules

-- | Convert a single rule declaration.
convertRuleDeclM ::
  Syntax.LRuleDecl Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertRuleDeclM lRuleDecl =
  mkItemM (Annotation.getLocA lRuleDecl) Nothing Nothing Doc.Empty Nothing ItemKind.Rule

-- | Convert class signatures.
convertClassSigsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.LSig Ghc.GhcPs] ->
  ConvertM [Located.Located Item.Item]
convertClassSigsM parentKey = fmap concat . traverse (convertClassSigM parentKey)

-- | Convert a single class signature.
convertClassSigM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LSig Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertClassSigM parentKey lSig = case SrcLoc.unLoc lSig of
  Syntax.ClassOpSig _ _ names _ ->
    let sig = extractSigSignature $ SrcLoc.unLoc lSig
     in Maybe.catMaybes <$> traverse (convertIdPM parentKey sig) names
  _ -> pure []

-- | Convert an identifier with parent key and signature.
convertIdPM ::
  Maybe ItemKey.ItemKey ->
  Maybe Text.Text ->
  Syntax.LIdP Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertIdPM parentKey sig lIdP =
  mkItemM (Annotation.getLocA lIdP) parentKey (Just $ extractIdPName lIdP) Doc.Empty sig ItemKind.ClassMethod

-- | Convert family declarations.
convertFamilyDeclsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.LFamilyDecl Ghc.GhcPs] ->
  ConvertM [Located.Located Item.Item]
convertFamilyDeclsM parentKey = fmap Maybe.catMaybes . traverse (convertFamilyDeclM parentKey)

-- | Convert a single family declaration.
convertFamilyDeclM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LFamilyDecl Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertFamilyDeclM parentKey lFamilyDecl =
  let famDecl = SrcLoc.unLoc lFamilyDecl
      itemKind = itemKindFromFamilyDecl famDecl
   in mkItemM
        (Annotation.getLocA lFamilyDecl)
        parentKey
        (Just $ extractFamilyDeclName famDecl)
        Doc.Empty
        Nothing
        itemKind

-- | Convert data definition constructors and deriving clauses.
convertDataDefnM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsDataDefn Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertDataDefnM parentKey dataDefn = do
  conItems <- concat <$> (traverse (convertConDeclM parentKey) . dataDefnConsList $ Syntax.dd_cons dataDefn)
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
  ConvertM [Located.Located Item.Item]
convertDerivingClausesM parentKey = fmap concat . traverse (convertDerivingClauseM parentKey)

-- | Convert a single deriving clause.
convertDerivingClauseM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LHsDerivingClause Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertDerivingClauseM parentKey lClause = do
  let clause = SrcLoc.unLoc lClause
      derivClauseTys = SrcLoc.unLoc $ Syntax.deriv_clause_tys clause
  convertDerivClauseTysM parentKey derivClauseTys

-- | Convert deriving clause types.
convertDerivClauseTysM ::
  Maybe ItemKey.ItemKey ->
  Syntax.DerivClauseTys Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertDerivClauseTysM parentKey dct = case dct of
  Syntax.DctSingle _ lSigTy -> Maybe.maybeToList <$> convertDerivedTypeM parentKey lSigTy
  Syntax.DctMulti _ lSigTys -> Maybe.catMaybes <$> traverse (convertDerivedTypeM parentKey) lSigTys

-- | Convert a derived type to an item.
convertDerivedTypeM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LHsSigType Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertDerivedTypeM parentKey lSigTy =
  mkItemM (Annotation.getLocA lSigTy) parentKey Nothing (extractDerivedTypeDoc lSigTy) Nothing ItemKind.DerivedInstance

-- | Extract documentation from a derived type.
extractDerivedTypeDoc :: Syntax.LHsSigType Ghc.GhcPs -> Doc.Doc
extractDerivedTypeDoc lSigTy =
  let sigTy = SrcLoc.unLoc lSigTy
      bodyTy = SrcLoc.unLoc $ Syntax.sig_body sigTy
   in case bodyTy of
        Syntax.HsDocTy _ _ lDoc -> convertLHsDoc lDoc
        _ -> Doc.Empty

-- | Convert a constructor declaration.
convertConDeclM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LConDecl Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertConDeclM parentKey lConDecl = do
  let conDecl = SrcLoc.unLoc lConDecl
      conDoc = extractConDeclDoc conDecl
      conSig = extractConDeclSignature conDecl
      conKind = constructorKind conDecl
  result <-
    mkItemWithKeyM
      (Annotation.getLocA lConDecl)
      parentKey
      (Just $ extractConDeclName conDecl)
      conDoc
      conSig
      conKind
  case result of
    Nothing -> pure []
    Just (constructorItem, key) -> do
      fieldItems <- extractFieldsFromConDeclM (Just key) conDecl
      pure $ [constructorItem] <> fieldItems

-- | Determine constructor kind.
constructorKind :: Syntax.ConDecl Ghc.GhcPs -> ItemKind.ItemKind
constructorKind conDecl = case conDecl of
  Syntax.ConDeclH98 {} -> ItemKind.DataConstructor
  Syntax.ConDeclGADT {} -> ItemKind.GADTConstructor

-- | Extract documentation from a constructor declaration.
extractConDeclDoc :: Syntax.ConDecl Ghc.GhcPs -> Doc.Doc
extractConDeclDoc conDecl = case conDecl of
  Syntax.ConDeclH98 {Syntax.con_doc = mDoc} ->
    maybe Doc.Empty convertLHsDoc mDoc
  Syntax.ConDeclGADT {Syntax.con_doc = mDoc} ->
    maybe Doc.Empty convertLHsDoc mDoc

-- | Extract signature from a constructor declaration.
extractConDeclSignature :: Syntax.ConDecl Ghc.GhcPs -> Maybe Text.Text
extractConDeclSignature = Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr

-- | Extract fields from a constructor declaration.
extractFieldsFromConDeclM ::
  Maybe ItemKey.ItemKey ->
  Syntax.ConDecl Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
extractFieldsFromConDeclM parentKey conDecl = case conDecl of
  Syntax.ConDeclH98 {Syntax.con_args = args} ->
    extractFieldsFromH98DetailsM parentKey args
  Syntax.ConDeclGADT {Syntax.con_g_args = gArgs} ->
    extractFieldsFromGADTDetailsM parentKey gArgs

-- | Extract fields from H98-style constructor details.
extractFieldsFromH98DetailsM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsConDeclH98Details Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
extractFieldsFromH98DetailsM parentKey details = case details of
  Syntax.PrefixCon _ -> pure []
  Syntax.InfixCon _ _ -> pure []
  Syntax.RecCon lFields -> convertConDeclFieldsM parentKey (SrcLoc.unLoc lFields)

-- | Extract fields from GADT-style constructor details.
extractFieldsFromGADTDetailsM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsConDeclGADTDetails Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
extractFieldsFromGADTDetailsM parentKey details = case details of
  Syntax.PrefixConGADT _ _ -> pure []
  Syntax.RecConGADT _ lFields -> convertConDeclFieldsM parentKey (SrcLoc.unLoc lFields)

-- | Convert a list of record fields.
convertConDeclFieldsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.LHsConDeclRecField Ghc.GhcPs] ->
  ConvertM [Located.Located Item.Item]
convertConDeclFieldsM parentKey = fmap concat . traverse (convertConDeclFieldM parentKey)

-- | Convert a single record field declaration to items (one per field name).
convertConDeclFieldM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LHsConDeclRecField Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertConDeclFieldM parentKey lField =
  let recField = SrcLoc.unLoc lField
      fieldSpec = Syntax.cdrf_spec recField
      doc = maybe Doc.Empty convertLHsDoc $ Syntax.cdf_doc fieldSpec
      sig = Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ Syntax.cdf_type fieldSpec
   in Maybe.catMaybes <$> traverse (convertFieldNameM parentKey doc sig) (Syntax.cdrf_names recField)

-- | Convert a single field name to an item.
convertFieldNameM ::
  Maybe ItemKey.ItemKey ->
  Doc.Doc ->
  Maybe Text.Text ->
  Syntax.LFieldOcc Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertFieldNameM parentKey doc sig lFieldOcc =
  mkItemM
    (Annotation.getLocA lFieldOcc)
    parentKey
    (Just $ extractFieldOccName lFieldOcc)
    doc
    sig
    ItemKind.RecordField

-- | Extract declaration name.
extractDeclName :: Syntax.LHsDecl Ghc.GhcPs -> Maybe ItemName.ItemName
extractDeclName lDecl = case SrcLoc.unLoc lDecl of
  Syntax.TyClD _ tyClDecl -> extractTyClDeclName tyClDecl
  Syntax.ValD _ bind -> extractBindName bind
  Syntax.SigD _ sig -> extractSigName sig
  Syntax.InstD _ inst -> extractInstDeclName inst
  Syntax.KindSigD _ kindSig -> Just $ extractStandaloneKindSigName kindSig
  _ -> Nothing

-- | Extract name from a standalone kind signature.
extractStandaloneKindSigName :: Syntax.StandaloneKindSig Ghc.GhcPs -> ItemName.ItemName
extractStandaloneKindSigName (Syntax.StandaloneKindSig _ lName _) = extractIdPName lName

-- | Extract signature from a kind signature.
extractKindSigSignature :: Syntax.StandaloneKindSig Ghc.GhcPs -> Text.Text
extractKindSigSignature = Text.pack . Outputable.showSDocUnsafe . Outputable.ppr

-- | Extract name from a type/class declaration.
extractTyClDeclName :: Syntax.TyClDecl Ghc.GhcPs -> Maybe ItemName.ItemName
extractTyClDeclName tyClDecl = case tyClDecl of
  Syntax.FamDecl _ famDecl -> Just $ extractFamilyDeclName famDecl
  Syntax.SynDecl {Syntax.tcdLName = lName} -> Just $ extractIdPName lName
  Syntax.DataDecl {Syntax.tcdLName = lName} -> Just $ extractIdPName lName
  Syntax.ClassDecl {Syntax.tcdLName = lName} -> Just $ extractIdPName lName

-- | Extract name from a family declaration.
extractFamilyDeclName :: Syntax.FamilyDecl Ghc.GhcPs -> ItemName.ItemName
extractFamilyDeclName famDecl = extractIdPName $ Syntax.fdLName famDecl

-- | Extract name from a binding.
extractBindName :: Syntax.HsBindLR Ghc.GhcPs Ghc.GhcPs -> Maybe ItemName.ItemName
extractBindName bind = case bind of
  Syntax.FunBind {Syntax.fun_id = lId} -> Just $ extractIdPName lId
  Syntax.PatBind {} -> Nothing
  Syntax.VarBind {} -> Nothing
  Syntax.PatSynBind _ patSyn -> Just $ extractPatSynName patSyn

-- | Extract name from a pattern synonym binding.
extractPatSynName :: Syntax.PatSynBind Ghc.GhcPs Ghc.GhcPs -> ItemName.ItemName
extractPatSynName patSyn = extractIdPName $ Syntax.psb_id patSyn

-- | Extract name from a signature.
extractSigName :: Syntax.Sig Ghc.GhcPs -> Maybe ItemName.ItemName
extractSigName sig = case sig of
  Syntax.TypeSig _ (lName : _) _ -> Just $ extractIdPName lName
  Syntax.PatSynSig _ (lName : _) _ -> Just $ extractIdPName lName
  Syntax.ClassOpSig _ _ (lName : _) _ -> Just $ extractIdPName lName
  _ -> Nothing

-- | Extract signature text from a Sig.
extractSigSignature :: Syntax.Sig Ghc.GhcPs -> Maybe Text.Text
extractSigSignature = Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr

-- | Extract name from an instance declaration.
extractInstDeclName :: Syntax.InstDecl Ghc.GhcPs -> Maybe ItemName.ItemName
extractInstDeclName inst = Just $ case inst of
  Syntax.ClsInstD _ clsInst ->
    ItemName.MkItemName . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $
      Syntax.cid_poly_ty clsInst
  Syntax.DataFamInstD _ dataFamInst ->
    ItemName.MkItemName . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $
      dataFamInst
  Syntax.TyFamInstD _ tyFamInst ->
    ItemName.MkItemName . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $
      tyFamInst

-- | Extract name from a constructor declaration.
extractConDeclName :: Syntax.ConDecl Ghc.GhcPs -> ItemName.ItemName
extractConDeclName conDecl = case conDecl of
  Syntax.ConDeclH98 {Syntax.con_name = lName} -> extractIdPName lName
  Syntax.ConDeclGADT {Syntax.con_names = lNames} ->
    extractIdPName $ NonEmpty.head lNames

-- | Extract name from an identifier.
extractIdPName :: Syntax.LIdP Ghc.GhcPs -> ItemName.ItemName
extractIdPName = ItemName.MkItemName . extractRdrName

-- | Extract name from a field occurrence.
extractFieldOccName :: Syntax.LFieldOcc Ghc.GhcPs -> ItemName.ItemName
extractFieldOccName lFieldOcc =
  let fieldOcc = SrcLoc.unLoc lFieldOcc
   in ItemName.MkItemName . extractRdrName $ Syntax.foLabel fieldOcc

-- | Merge items that share the same name.
mergeItemsByName :: [Located.Located Item.Item] -> [Located.Located Item.Item]
mergeItemsByName items =
  let mergeMap = buildMergeMap items
      keyRemapping = buildKeyRemapping items mergeMap
   in Maybe.mapMaybe (applyMerge mergeMap keyRemapping) items

-- | Build a map from item name to the merged item for that name.
buildMergeMap ::
  [Located.Located Item.Item] ->
  Map.Map ItemName.ItemName (Located.Located Item.Item)
buildMergeMap items =
  let namedCandidates =
        Maybe.mapMaybe
          (\item -> fmap (\n -> (n, item NonEmpty.:| [])) . Item.name $ Located.value item)
          (filter isMergeCandidate items)
      groups = Map.fromListWith (<>) namedCandidates
   in Map.map mergeItemGroup groups

-- | Check if an item is eligible for merging.
isMergeCandidate :: Located.Located Item.Item -> Bool
isMergeCandidate item =
  let val = Located.value item
   in Maybe.isNothing (Item.parentKey val) && Maybe.isJust (Item.name val)

-- | Merge a group of items sharing the same name into a single item.
mergeItemGroup :: NonEmpty.NonEmpty (Located.Located Item.Item) -> Located.Located Item.Item
mergeItemGroup (single NonEmpty.:| []) = single
mergeItemGroup group =
  let sorted = NonEmpty.sortWith Located.location group
      firstItem = NonEmpty.head sorted
      combinedDoc = foldr (appendDoc . Item.documentation . Located.value) Doc.Empty sorted
      combinedSig =
        Maybe.listToMaybe . Maybe.mapMaybe (Item.signature . Located.value) $ NonEmpty.toList sorted
      mergedItem =
        (Located.value firstItem)
          { Item.documentation = combinedDoc,
            Item.signature = combinedSig
          }
   in firstItem {Located.value = mergedItem}

-- | Build a mapping from old item keys to new merged item keys.
buildKeyRemapping ::
  [Located.Located Item.Item] ->
  Map.Map ItemName.ItemName (Located.Located Item.Item) ->
  Map.Map ItemKey.ItemKey ItemKey.ItemKey
buildKeyRemapping items mergeMap =
  Map.fromList $ concatMap (findRemapping mergeMap) items

-- | Find key remappings for a single item.
findRemapping ::
  Map.Map ItemName.ItemName (Located.Located Item.Item) ->
  Located.Located Item.Item ->
  [(ItemKey.ItemKey, ItemKey.ItemKey)]
findRemapping mergeMap item =
  let val = Located.value item
      itemKey = Item.key val
   in case Item.name val of
        Nothing -> []
        Just name ->
          case Map.lookup name mergeMap of
            Nothing -> []
            Just merged ->
              let mergedKey = Item.key $ Located.value merged
               in if itemKey /= mergedKey && isMergeCandidate item
                    then [(itemKey, mergedKey)]
                    else []

-- | Apply merge: either drop (remapped away) or replace with merged version.
applyMerge ::
  Map.Map ItemName.ItemName (Located.Located Item.Item) ->
  Map.Map ItemKey.ItemKey ItemKey.ItemKey ->
  Located.Located Item.Item ->
  Maybe (Located.Located Item.Item)
applyMerge mergeMap keyRemapping item =
  let val = Located.value item
      itemKey = Item.key val
   in if Map.member itemKey keyRemapping
        then Nothing
        else
          let updatedItem = updateParentKey keyRemapping item
           in case Item.name val of
                Nothing -> Just updatedItem
                Just name ->
                  if not (isMergeCandidate item)
                    then Just updatedItem
                    else case Map.lookup name mergeMap of
                      Nothing -> Just updatedItem
                      Just merged ->
                        if Item.key (Located.value merged) == itemKey
                          then Just merged
                          else Just updatedItem

-- | Update an item's parent key according to the remapping.
updateParentKey ::
  Map.Map ItemKey.ItemKey ItemKey.ItemKey ->
  Located.Located Item.Item ->
  Located.Located Item.Item
updateParentKey keyRemapping locatedItem =
  let val = Located.value locatedItem
   in case Item.parentKey val of
        Nothing -> locatedItem
        Just pk ->
          case Map.lookup pk keyRemapping of
            Nothing -> locatedItem
            Just newPk ->
              locatedItem
                { Located.value =
                    val {Item.parentKey = Just newPk}
                }

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'fromGhc $ do
    Spec.it s "converts language correctly" $ do
      Spec.assertEq s (languageFromGhc Session.Haskell98) (Language.MkLanguage $ Text.pack "Haskell98")

    Spec.it s "converts extension correctly" $ do
      let ext = extensionFromGhc GhcExtension.Cpp
      Spec.assertEq s (Extension.unwrap ext) (Text.pack "Cpp")
