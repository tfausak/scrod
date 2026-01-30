module Scrod.Unstable.Convert where

import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
import qualified Data.Void as Void
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
import qualified GHC.Parser.Errors.Types as Errors
import qualified GHC.Types.Error as Error
import qualified GHC.Types.Name.Reader as Reader
import qualified GHC.Types.SourceError as SourceError
import qualified GHC.Types.SourceText as SourceText
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Unit.Module.Warnings as Warnings
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as Syntax
import qualified PackageInfo_scrod as PackageInfo
import qualified Scrod.Unstable.Extra.OnOff as OnOff
import qualified Scrod.Unstable.Type.Category as Category
import qualified Scrod.Unstable.Type.ConversionState as ConversionState
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Example as Example
import qualified Scrod.Unstable.Type.Export as Export
import qualified Scrod.Unstable.Type.ExportIdentifier as ExportIdentifier
import qualified Scrod.Unstable.Type.ExportName as ExportName
import qualified Scrod.Unstable.Type.ExportNameKind as ExportNameKind
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Header as Header
import qualified Scrod.Unstable.Type.Hyperlink as Hyperlink
import qualified Scrod.Unstable.Type.Identifier as Identifier
import qualified Scrod.Unstable.Type.Interface as Interface
import qualified Scrod.Unstable.Type.Item as Item
import qualified Scrod.Unstable.Type.ItemKey as ItemKey
import qualified Scrod.Unstable.Type.ItemName as ItemName
import qualified Scrod.Unstable.Type.Language as Language
import qualified Scrod.Unstable.Type.Level as Level
import qualified Scrod.Unstable.Type.Located as Located
import qualified Scrod.Unstable.Type.Location as Location
import qualified Scrod.Unstable.Type.ModLink as ModLink
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.Namespace as Namespace
import qualified Scrod.Unstable.Type.PackageName as PackageName
import qualified Scrod.Unstable.Type.Picture as Picture
import qualified Scrod.Unstable.Type.Section as Section
import qualified Scrod.Unstable.Type.Since as Since
import qualified Scrod.Unstable.Type.Subordinates as Subordinates
import qualified Scrod.Unstable.Type.Table as Table
import qualified Scrod.Unstable.Type.TableCell as TableCell
import qualified Scrod.Unstable.Type.Version as Version
import qualified Scrod.Unstable.Type.Warning as Warning

-- | Monad for item conversion with auto-incrementing keys.
type ConvertM a = State.State ConversionState.ConversionState a

-- | Allocate a new unique key.
allocateKey :: ConvertM ItemKey.ItemKey
allocateKey = State.state ConversionState.allocateKey

-- | Run the conversion monad and extract the result.
runConvert :: ConvertM a -> a
runConvert = flip State.evalState ConversionState.initialState

-- | Create an Item from a source span with the given properties.
-- This is the common helper used by all item creation functions.
mkItemM ::
  SrcLoc.SrcSpan ->
  Maybe ItemKey.ItemKey ->
  Maybe ItemName.ItemName ->
  Doc.Doc ->
  ConvertM (Maybe (Located.Located Item.Item))
mkItemM srcSpan parentKey itemName doc =
  fmap (fmap fst) $ mkItemWithKeyM srcSpan parentKey itemName doc

-- | Create an Item and return both the item and its allocated key.
-- Useful when the key is needed for creating child items.
mkItemWithKeyM ::
  SrcLoc.SrcSpan ->
  Maybe ItemKey.ItemKey ->
  Maybe ItemName.ItemName ->
  Doc.Doc ->
  ConvertM (Maybe (Located.Located Item.Item, ItemKey.ItemKey))
mkItemWithKeyM srcSpan parentKey itemName doc =
  case Location.fromSrcSpan srcSpan of
    Nothing -> pure Nothing
    Just location -> do
      key <- allocateKey
      pure $
        Just
          ( Located.MkLocated
              { Located.location = location,
                Located.value =
                  Item.MkItem
                    { Item.key = key,
                      Item.parentKey = parentKey,
                      Item.name = itemName,
                      Item.documentation = doc
                    }
              },
            key
          )

convert ::
  [String] ->
  Either
    (Either SourceError.SourceError (Error.Messages Errors.PsMessage))
    ( (Maybe Session.Language, [DynFlags.OnOff GhcExtension.Extension]),
      SrcLoc.Located (Hs.HsModule Ghc.GhcPs)
    ) ->
  Either String Interface.Interface
convert cliExtensions input = case input of
  Left (Left sourceError) ->
    Left $ Exception.displayException sourceError
  Left (Right messages) ->
    Left . Outputable.showSDocUnsafe $ Outputable.ppr messages
  Right ((language, sourceExtensions), lHsModule) -> do
    version <- maybe (Left "invalid version") pure $ Version.fromBase PackageInfo.version
    let mergedExtensions = sourceExtensions <> parseCliExtensions cliExtensions
    pure
      Interface.MkInterface
        { Interface.version = version,
          Interface.language = fmap Language.fromGhc language,
          Interface.extensions = extensionsToMap mergedExtensions,
          Interface.documentation = extractModuleDocumentation lHsModule,
          Interface.since = extractModuleSince lHsModule,
          Interface.name = extractModuleName lHsModule,
          Interface.warning = extractModuleWarning lHsModule,
          Interface.exports = extractModuleExports lHsModule,
          Interface.items = extractItems lHsModule
        }

parseCliExtensions :: [String] -> [DynFlags.OnOff GhcExtension.Extension]
parseCliExtensions = Maybe.mapMaybe parseOneExtension
  where
    parseOneExtension s
      | "No" `List.isPrefixOf` s = fmap DynFlags.Off $ lookupExt (drop 2 s)
      | otherwise = fmap DynFlags.On $ lookupExt s
    lookupExt name = List.lookup name extensionMap
    extensionMap = fmap (\x -> (Session.flagSpecName x, Session.flagSpecFlag x)) Session.xFlags

extensionsToMap ::
  [DynFlags.OnOff GhcExtension.Extension] ->
  Map.Map Extension.Extension Bool
extensionsToMap =
  Map.fromListWith (\_ x -> x)
    . fmap (Tuple.swap . fmap Extension.fromGhc . OnOff.onOff ((,) True) ((,) False))

extractModuleName ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe (Located.Located ModuleName.ModuleName)
extractModuleName lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
  lModuleName <- Syntax.hsmodName hsModule
  let srcSpan = Annotation.getLocA lModuleName
      moduleName = ModuleName.fromGhc $ SrcLoc.unLoc lModuleName
  Located.fromGhc $ SrcLoc.L srcSpan moduleName

extractModuleDocumentation ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Doc.Doc
extractModuleDocumentation =
  maybe mempty parseDoc
    . extractRawDocString

extractRawDocString ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe String
extractRawDocString lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
      xModulePs = Syntax.hsmodExt hsModule
  lHsDoc <- Hs.hsmodHaddockModHeader xModulePs
  let hsDoc = SrcLoc.unLoc lHsDoc
      hsDocString = HsDoc.hsDocString hsDoc
  pure $ DocString.renderHsDocString hsDocString

extractModuleSince ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe Since.Since
extractModuleSince lHsModule = do
  meta <- extractModuleMeta lHsModule
  haddockVersion <- Haddock._version meta
  version <- Version.fromHaddock haddockVersion
  pure
    Since.MkSince
      { Since.package = fmap PackageName.fromString $ Haddock._package meta,
        Since.version = version
      }

extractModuleMeta ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe Haddock.Meta
extractModuleMeta =
  fmap Haddock._meta
    . extractModuleMetaDoc

extractModuleMetaDoc ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe (Haddock.MetaDoc m Haddock.Identifier)
extractModuleMetaDoc lHsModule = do
  rendered <- extractRawDocString lHsModule
  pure $ Haddock.parseParas Nothing rendered

extractModuleWarning ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe Warning.Warning
extractModuleWarning lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
      xModulePs = Syntax.hsmodExt hsModule
  lWarningTxt <- Hs.hsmodDeprecMessage xModulePs
  let warningTxt = SrcLoc.unLoc lWarningTxt
  pure $ warningTxtToWarning warningTxt

extractModuleExports ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Maybe [Export.Export]
extractModuleExports lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
  lExports <- Syntax.hsmodExports hsModule
  let exports = SrcLoc.unLoc lExports
  pure $ fmap convertIE exports

extractItems ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  [Located.Located Item.Item]
extractItems lHsModule =
  let rawItems = runConvert $ extractItemsM lHsModule
   in mergeItemsByName rawItems

-- | Merge items that share the same name.
-- This combines type signatures with their corresponding declarations.
-- Uses the earliest source location and concatenates documentation.
-- Only merges top-level items (those without a parent key).
-- Maintains source order of items by placing merged items at their earliest location.
-- Updates child items' parentKey references when their parent is merged.
mergeItemsByName :: [Located.Located Item.Item] -> [Located.Located Item.Item]
mergeItemsByName items =
  let -- Build a map of names to merge: name -> (earliest location, combined item)
      mergeMap = buildMergeMap items
      -- Build map from removed key -> merged key (for updating parentKey references)
      keyRemapping = buildKeyRemapping items mergeMap
      -- Apply merges, filter out removed items, and update parentKey references
      result = Maybe.mapMaybe (applyMerge mergeMap keyRemapping) items
   in result
  where
    -- Build map from name to merged item info
    buildMergeMap ::
      [Located.Located Item.Item] ->
      Map.Map ItemName.ItemName (Located.Located Item.Item)
    buildMergeMap is =
      let -- Only consider top-level named items, extracting name safely
          namedCandidates =
            Maybe.mapMaybe
              (\item -> fmap (\n -> (n, [item])) (Item.name (Located.value item)))
              (filter isMergeCandidate is)
          -- Group by name
          groups = Map.fromListWith (<>) namedCandidates
       in -- Merge each group (only groups with multiple items need merging)
          Map.map mergeItemGroup groups

    isMergeCandidate :: Located.Located Item.Item -> Bool
    isMergeCandidate item =
      let val = Located.value item
       in Maybe.isNothing (Item.parentKey val) && Maybe.isJust (Item.name val)

    mergeItemGroup :: [Located.Located Item.Item] -> Located.Located Item.Item
    mergeItemGroup [] = error "mergeItemGroup: empty group"
    mergeItemGroup [single] = single
    mergeItemGroup group =
      let -- Sort by location to find earliest
          sorted = List.sortOn Located.location group
       in case sorted of
            [] -> error "mergeItemGroup: sorted empty"
            (firstItem : _) ->
              let -- Concatenate all documentation in source order
                  combinedDoc = mconcat $ fmap (Item.documentation . Located.value) sorted
                  -- Use earliest location, first item's key and parentKey
                  mergedItem =
                    (Located.value firstItem)
                      { Item.documentation = combinedDoc
                      }
               in firstItem {Located.value = mergedItem}

    -- Build map from removed key -> merged key (for updating parentKey references)
    buildKeyRemapping ::
      [Located.Located Item.Item] ->
      Map.Map ItemName.ItemName (Located.Located Item.Item) ->
      Map.Map ItemKey.ItemKey ItemKey.ItemKey
    buildKeyRemapping is mergeMap =
      Map.fromList $ concatMap (findRemapping mergeMap) is

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
                  let mergedKey = Item.key (Located.value merged)
                   in if itemKey /= mergedKey && isMergeCandidate item
                        then [(itemKey, mergedKey)]
                        else []

    -- Apply merge: replace with merged item if this is the primary, or filter out if removed
    -- Also updates parentKey references for child items
    applyMerge ::
      Map.Map ItemName.ItemName (Located.Located Item.Item) ->
      Map.Map ItemKey.ItemKey ItemKey.ItemKey ->
      Located.Located Item.Item ->
      Maybe (Located.Located Item.Item)
    applyMerge mergeMap keyRemapping item =
      let val = Located.value item
          itemKey = Item.key val
       in if Map.member itemKey keyRemapping
            then Nothing -- This item was merged into another
            else
              let -- Update parentKey if it points to a removed key
                  updatedItem = updateParentKey keyRemapping item
               in case Item.name val of
                    Nothing -> Just updatedItem -- Unnamed items pass through
                    Just name ->
                      if not (isMergeCandidate item)
                        then Just updatedItem -- Child items pass through (with updated parentKey)
                        else case Map.lookup name mergeMap of
                          Nothing -> Just updatedItem
                          Just merged ->
                            if Item.key (Located.value merged) == itemKey
                              then Just merged -- This is the primary, use merged version
                              else Just updatedItem -- Shouldn't happen, but pass through

    -- Update an item's parentKey if it points to a removed key
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
                Nothing -> locatedItem -- parentKey not remapped
                Just newPk ->
                  locatedItem
                    { Located.value =
                        val {Item.parentKey = Just newPk}
                    }

extractItemsM ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  ConvertM [Located.Located Item.Item]
extractItemsM lHsModule = do
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
      declsWithDocs = associateDocs decls
  fmap concat $ traverse (uncurry convertDeclWithDocMaybeM) declsWithDocs

-- | Associate documentation comments with their target declarations.
-- DocCommentNext applies to the next non-doc declaration.
-- DocCommentPrev applies to the previous non-doc declaration.
associateDocs ::
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associateDocs decls =
  let -- First pass: handle DocCommentNext (forward)
      withNextDocs = associateNextDocs decls
      -- Second pass: handle DocCommentPrev (backward)
      withAllDocs = associatePrevDocs withNextDocs
   in withAllDocs

-- | Associate DocCommentNext with the following declaration.
associateNextDocs ::
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associateNextDocs = go mempty
  where
    go :: Doc.Doc -> [Syntax.LHsDecl Ghc.GhcPs] -> [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
    go _ [] = []
    go pendingDoc (lDecl : rest) = case SrcLoc.unLoc lDecl of
      Syntax.DocD _ (Hs.DocCommentNext lDoc) ->
        -- Combine with any existing pending doc
        let newDoc = pendingDoc <> convertLHsDoc lDoc
         in go newDoc rest
      Syntax.DocD _ (Hs.DocCommentPrev _) ->
        -- Skip DocCommentPrev in this pass, but keep the declaration
        (Doc.Empty, lDecl) : go mempty rest
      _ ->
        (pendingDoc, lDecl) : go mempty rest

-- | Associate DocCommentPrev with the preceding declaration.
associatePrevDocs ::
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associatePrevDocs = reverse . go . reverse
  where
    go :: [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)] -> [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
    go [] = []
    go ((doc, lDecl) : rest) = case SrcLoc.unLoc lDecl of
      Syntax.DocD _ (Hs.DocCommentPrev lDoc) ->
        -- Apply to the next item in the reversed list (which is the previous item originally)
        let prevDoc = convertLHsDoc lDoc
         in applyPrevDoc prevDoc (go rest)
      _ ->
        (doc, lDecl) : go rest

    applyPrevDoc :: Doc.Doc -> [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)] -> [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
    applyPrevDoc _ [] = []
    applyPrevDoc prevDoc ((existingDoc, lDecl) : rest) = case SrcLoc.unLoc lDecl of
      -- Skip over other doc declarations
      Syntax.DocD {} -> (existingDoc, lDecl) : applyPrevDoc prevDoc rest
      -- Apply to first non-doc declaration
      _ -> (existingDoc <> prevDoc, lDecl) : rest

-- | Convert a declaration with documentation (monadic version).
convertDeclWithDocMaybeM ::
  Doc.Doc ->
  Syntax.LHsDecl Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertDeclWithDocMaybeM doc lDecl = case SrcLoc.unLoc lDecl of
  Syntax.TyClD _ tyClDecl -> convertTyClDeclWithDocM doc lDecl tyClDecl
  Syntax.RuleD _ ruleDecls -> convertRuleDeclsM ruleDecls
  Syntax.DocD {} -> fmap Maybe.maybeToList $ convertDeclSimpleM lDecl
  Syntax.SigD _ sig -> convertSigDeclM doc lDecl sig
  _ -> fmap Maybe.maybeToList $ convertDeclWithDocM Nothing doc (extractDeclName lDecl) lDecl

-- | Convert a signature declaration, handling multi-name signatures.
-- For signatures like "x, y :: Int", this creates a separate item for each name.
convertSigDeclM ::
  Doc.Doc ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Syntax.Sig Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertSigDeclM doc lDecl sig = case sig of
  Syntax.TypeSig _ names _ ->
    fmap Maybe.catMaybes $ traverse (convertSigNameM doc) names
  Syntax.PatSynSig _ names _ ->
    fmap Maybe.catMaybes $ traverse (convertSigNameM doc) names
  _ -> fmap Maybe.maybeToList $ convertDeclWithDocM Nothing doc (extractSigName sig) lDecl

-- | Convert a single name from a signature into an item.
-- Uses the individual name's location for accuracy in multi-name signatures.
convertSigNameM ::
  Doc.Doc ->
  Syntax.LIdP Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertSigNameM doc lName =
  mkItemM (Annotation.getLocA lName) Nothing (Just $ extractIdPName lName) doc

-- | Convert a type/class declaration with documentation.
convertTyClDeclWithDocM ::
  Doc.Doc ->
  Syntax.LHsDecl Ghc.GhcPs ->
  Syntax.TyClDecl Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertTyClDeclWithDocM doc lDecl tyClDecl = case tyClDecl of
  Syntax.DataDecl _ _ _ _ dataDefn -> do
    parentItem <- convertDeclWithDocM Nothing doc (extractTyClDeclName tyClDecl) lDecl
    let parentKey = fmap (Item.key . Located.value) parentItem
    childItems <- convertDataDefnM parentKey dataDefn
    pure $ Maybe.maybeToList parentItem <> childItems
  Syntax.ClassDecl {Syntax.tcdSigs = sigs, Syntax.tcdATs = ats} -> do
    parentItem <- convertDeclWithDocM Nothing doc (extractTyClDeclName tyClDecl) lDecl
    let parentKey = fmap (Item.key . Located.value) parentItem
    methodItems <- convertClassSigsM parentKey sigs
    familyItems <- convertFamilyDeclsM parentKey ats
    pure $ Maybe.maybeToList parentItem <> methodItems <> familyItems
  _ -> fmap Maybe.maybeToList $ convertDeclWithDocM Nothing doc (extractTyClDeclName tyClDecl) lDecl

convertDeclSimpleM ::
  Syntax.LHsDecl Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertDeclSimpleM = convertDeclWithDocM Nothing mempty Nothing

convertDeclWithDocM ::
  Maybe ItemKey.ItemKey ->
  Doc.Doc ->
  Maybe ItemName.ItemName ->
  Syntax.LHsDecl Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertDeclWithDocM parentKey doc itemName lDecl =
  mkItemM (Annotation.getLocA lDecl) parentKey itemName doc

convertRuleDeclsM ::
  Syntax.RuleDecls Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertRuleDeclsM (Syntax.HsRules _ rules) = fmap Maybe.catMaybes $ traverse convertRuleDeclM rules

convertRuleDeclM ::
  Syntax.LRuleDecl Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertRuleDeclM lRuleDecl =
  mkItemM (Annotation.getLocA lRuleDecl) Nothing Nothing mempty

convertClassSigsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.LSig Ghc.GhcPs] ->
  ConvertM [Located.Located Item.Item]
convertClassSigsM parentKey = fmap concat . traverse (convertClassSigM parentKey)

convertClassSigM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LSig Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertClassSigM parentKey lSig = case SrcLoc.unLoc lSig of
  Syntax.ClassOpSig _ _ names _ -> fmap Maybe.catMaybes $ traverse (convertIdPM parentKey) names
  _ -> pure []

convertIdPM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LIdP Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertIdPM parentKey lIdP =
  mkItemM (Annotation.getLocA lIdP) parentKey (Just $ extractIdPName lIdP) mempty

convertFamilyDeclsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.LFamilyDecl Ghc.GhcPs] ->
  ConvertM [Located.Located Item.Item]
convertFamilyDeclsM parentKey = fmap Maybe.catMaybes . traverse (convertFamilyDeclM parentKey)

convertFamilyDeclM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LFamilyDecl Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertFamilyDeclM parentKey lFamilyDecl =
  mkItemM
    (Annotation.getLocA lFamilyDecl)
    parentKey
    (Just $ extractFamilyDeclName (SrcLoc.unLoc lFamilyDecl))
    mempty

convertDataDefnM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsDataDefn Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertDataDefnM parentKey dataDefn = do
  conItems <- fmap concat $ traverse (convertConDeclM parentKey) (dataDefnConsList (Syntax.dd_cons dataDefn))
  derivItems <- convertDerivingClausesM parentKey (Syntax.dd_derivs dataDefn)
  pure $ conItems <> derivItems

convertDerivingClausesM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsDeriving Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertDerivingClausesM parentKey = fmap concat . traverse (convertDerivingClauseM parentKey)

convertDerivingClauseM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LHsDerivingClause Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertDerivingClauseM parentKey lClause = do
  let clause = SrcLoc.unLoc lClause
      derivClauseTys = SrcLoc.unLoc $ Syntax.deriv_clause_tys clause
  convertDerivClauseTysM parentKey derivClauseTys

convertDerivClauseTysM ::
  Maybe ItemKey.ItemKey ->
  Syntax.DerivClauseTys Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertDerivClauseTysM parentKey dct = case dct of
  Syntax.DctSingle _ lSigTy -> fmap Maybe.maybeToList $ convertDerivedTypeM parentKey lSigTy
  Syntax.DctMulti _ lSigTys -> fmap Maybe.catMaybes $ traverse (convertDerivedTypeM parentKey) lSigTys

convertDerivedTypeM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LHsSigType Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
convertDerivedTypeM parentKey lSigTy =
  mkItemM (Annotation.getLocA lSigTy) parentKey Nothing mempty

dataDefnConsList ::
  Syntax.DataDefnCons a ->
  [a]
dataDefnConsList ddc = case ddc of
  Syntax.NewTypeCon con -> [con]
  Syntax.DataTypeCons _ cons -> cons

convertConDeclM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LConDecl Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
convertConDeclM parentKey lConDecl = do
  let conDecl = SrcLoc.unLoc lConDecl
      conDoc = extractConDeclDoc conDecl
  result <-
    mkItemWithKeyM
      (Annotation.getLocA lConDecl)
      parentKey
      (Just $ extractConDeclName conDecl)
      conDoc
  case result of
    Nothing -> pure []
    Just (constructorItem, key) -> do
      fieldItems <- extractFieldsFromConDeclM (Just key) conDecl
      pure $ [constructorItem] <> fieldItems

extractConDeclDoc ::
  Syntax.ConDecl Ghc.GhcPs ->
  Doc.Doc
extractConDeclDoc conDecl = case conDecl of
  Syntax.ConDeclH98 {Syntax.con_doc = mDoc} ->
    maybe mempty convertLHsDoc mDoc
  Syntax.ConDeclGADT {Syntax.con_doc = mDoc} ->
    maybe mempty convertLHsDoc mDoc

extractFieldsFromConDeclM ::
  Maybe ItemKey.ItemKey ->
  Syntax.ConDecl Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
extractFieldsFromConDeclM parentKey conDecl = case conDecl of
  Syntax.ConDeclH98 {Syntax.con_args = args} ->
    extractFieldsFromH98DetailsM parentKey args
  Syntax.ConDeclGADT {Syntax.con_g_args = gArgs} ->
    extractFieldsFromGADTDetailsM parentKey gArgs

extractFieldsFromH98DetailsM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsConDeclH98Details Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
extractFieldsFromH98DetailsM parentKey details = case details of
  Syntax.RecCon lFieldsRec ->
    extractFieldItemsM parentKey $ SrcLoc.unLoc lFieldsRec
  _ -> pure []

extractFieldsFromGADTDetailsM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsConDeclGADTDetails Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
extractFieldsFromGADTDetailsM parentKey details = case details of
  Syntax.RecConGADT _ lFieldsRec ->
    extractFieldItemsM parentKey $ SrcLoc.unLoc lFieldsRec
  _ -> pure []

extractFieldItemsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.LConDeclField Ghc.GhcPs] ->
  ConvertM [Located.Located Item.Item]
extractFieldItemsM parentKey = fmap concat . traverse (extractFieldItemsFromConDeclFieldM parentKey)

extractFieldItemsFromConDeclFieldM ::
  Maybe ItemKey.ItemKey ->
  Syntax.LConDeclField Ghc.GhcPs ->
  ConvertM [Located.Located Item.Item]
extractFieldItemsFromConDeclFieldM parentKey lField = do
  let field = SrcLoc.unLoc lField
      fieldNames = Syntax.cd_fld_names field
      fieldDoc = maybe mempty convertLHsDoc (Syntax.cd_fld_doc field)
  fmap Maybe.catMaybes $ traverse (extractFieldItemM parentKey fieldDoc) fieldNames

extractFieldItemM ::
  Maybe ItemKey.ItemKey ->
  Doc.Doc ->
  Syntax.LFieldOcc Ghc.GhcPs ->
  ConvertM (Maybe (Located.Located Item.Item))
extractFieldItemM parentKey doc lFieldOcc =
  mkItemM
    (Annotation.getLocA lFieldOcc)
    parentKey
    (Just $ extractFieldOccName lFieldOcc)
    doc

-- | Extract name from a top-level declaration.
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

-- | Extract name from an instance declaration (returns Nothing for now).
extractInstDeclName :: Syntax.InstDecl Ghc.GhcPs -> Maybe ItemName.ItemName
extractInstDeclName _ = Nothing

-- | Extract name from a constructor declaration.
extractConDeclName :: Syntax.ConDecl Ghc.GhcPs -> ItemName.ItemName
extractConDeclName conDecl = case conDecl of
  Syntax.ConDeclH98 {Syntax.con_name = lName} -> extractIdPName lName
  Syntax.ConDeclGADT {Syntax.con_names = lNames} ->
    -- NonEmpty list, take the first one
    extractIdPName $ NonEmpty.head lNames

-- | Extract name from an identifier.
extractIdPName :: Syntax.LIdP Ghc.GhcPs -> ItemName.ItemName
extractIdPName = ItemName.MkItemName . extractRdrName

-- | Extract name from a field occurrence.
extractFieldOccName :: Syntax.LFieldOcc Ghc.GhcPs -> ItemName.ItemName
extractFieldOccName lFieldOcc =
  let fieldOcc = SrcLoc.unLoc lFieldOcc
   in ItemName.MkItemName . extractRdrName $ Syntax.foLabel fieldOcc

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
          ExportIdentifier.doc = fmap convertExportDoc mDoc
        }
  Syntax.IEThingAbs (mLWarning, _) lName mDoc ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name = convertWrappedName lName,
          ExportIdentifier.subordinates = Nothing,
          ExportIdentifier.warning = convertExportWarning mLWarning,
          ExportIdentifier.doc = fmap convertExportDoc mDoc
        }
  Syntax.IEThingAll (mLWarning, _) lName mDoc ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name = convertWrappedName lName,
          ExportIdentifier.subordinates = Just Subordinates.MkSubordinates {Subordinates.wildcard = True, Subordinates.explicit = []},
          ExportIdentifier.warning = convertExportWarning mLWarning,
          ExportIdentifier.doc = fmap convertExportDoc mDoc
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
          ExportIdentifier.doc = fmap convertExportDoc mDoc
        }
  Syntax.IEModuleContents (mLWarning, _) lModName ->
    Export.Identifier
      ExportIdentifier.MkExportIdentifier
        { ExportIdentifier.name =
            ExportName.MkExportName
              { ExportName.kind = Just ExportNameKind.Module,
                ExportName.name = ModuleName.value (ModuleName.fromGhc $ SrcLoc.unLoc lModName)
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
              { Header.level = Maybe.fromMaybe Level.One (Level.fromIntegral level),
                Header.title = convertLHsDoc lDoc
              }
        }
  Syntax.IEDoc _ lDoc ->
    Export.Doc (convertLHsDoc lDoc)
  Syntax.IEDocNamed _ name ->
    Export.DocNamed (Text.pack name)

hasWildcard :: ImpExp.IEWildcard -> Bool
hasWildcard wildcard = case wildcard of
  ImpExp.NoIEWildcard -> False
  ImpExp.IEWildcard _ -> True

convertExportWarning ::
  Maybe (SrcLoc.GenLocated l (Warnings.WarningTxt Ghc.GhcPs)) ->
  Maybe Warning.Warning
convertExportWarning = fmap (warningTxtToWarning . SrcLoc.unLoc)

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

extractRdrName ::
  SrcLoc.GenLocated l Reader.RdrName ->
  Text.Text
extractRdrName =
  Text.pack
    . Outputable.showSDocUnsafe
    . Outputable.ppr
    . SrcLoc.unLoc

convertExportDoc ::
  SrcLoc.GenLocated l (HsDoc.WithHsDocIdentifiers DocString.HsDocString Ghc.GhcPs) ->
  Doc.Doc
convertExportDoc lDoc =
  let hsDoc = SrcLoc.unLoc lDoc
      hsDocString = HsDoc.hsDocString hsDoc
      rendered = DocString.renderHsDocString hsDocString
   in parseDoc rendered

convertLHsDoc ::
  SrcLoc.GenLocated l (HsDoc.WithHsDocIdentifiers DocString.HsDocString Ghc.GhcPs) ->
  Doc.Doc
convertLHsDoc = convertExportDoc

warningTxtToWarning :: Warnings.WarningTxt Ghc.GhcPs -> Warning.Warning
warningTxtToWarning warningTxt =
  Warning.MkWarning
    { Warning.category = Category.fromGhc $ Warnings.warningTxtCategory warningTxt,
      Warning.value = Text.intercalate (Text.singleton '\n') . fmap extractMessage $ Warnings.warningTxtMessage warningTxt
    }

extractMessage :: SrcLoc.GenLocated l (HsDoc.WithHsDocIdentifiers SourceText.StringLiteral Ghc.GhcPs) -> Text.Text
extractMessage =
  Text.pack
    . FastString.unpackFS
    . SourceText.sl_fs
    . HsDoc.hsDocString
    . SrcLoc.unLoc

-- | Convert Haddock's Namespace to our Namespace.
fromHaddockNamespace :: Haddock.Namespace -> Maybe Namespace.Namespace
fromHaddockNamespace ns = case ns of
  Haddock.Value -> Just Namespace.Value
  Haddock.Type -> Just Namespace.Type
  Haddock.None -> Nothing

-- | Convert a Haddock Identifier to our Identifier.
-- Used with 'Haddock.overIdentifier'.
convertIdentifier :: Haddock.Namespace -> String -> Maybe Identifier.Identifier
convertIdentifier ns str =
  Just
    Identifier.MkIdentifier
      { Identifier.namespace = fromHaddockNamespace ns,
        Identifier.value = Text.pack str
      }

-- | Convert from Haddock's parsed doc to our simplified Doc type.
fromHaddock :: Haddock.DocH Void.Void Identifier.Identifier -> Doc.Doc
fromHaddock doc = case doc of
  Haddock.DocEmpty -> mempty
  Haddock.DocAppend a b -> Doc.Append (fromHaddock a) (fromHaddock b)
  Haddock.DocString s -> Doc.String (Text.pack s)
  Haddock.DocParagraph d -> Doc.Paragraph (fromHaddock d)
  Haddock.DocIdentifier i -> Doc.Identifier i
  Haddock.DocIdentifierUnchecked v -> Void.absurd v
  Haddock.DocModule ml ->
    Doc.Module
      ModLink.MkModLink
        { ModLink.name = ModuleName.fromString (Haddock.modLinkName ml),
          ModLink.label = fmap fromHaddock (Haddock.modLinkLabel ml)
        }
  Haddock.DocWarning _ -> mempty -- `DocWarning` is never found in markup.
  Haddock.DocEmphasis d -> Doc.Emphasis (fromHaddock d)
  Haddock.DocMonospaced d -> Doc.Monospaced (fromHaddock d)
  Haddock.DocBold d -> Doc.Bold (fromHaddock d)
  Haddock.DocUnorderedList ds -> Doc.UnorderedList (fmap fromHaddock ds)
  Haddock.DocOrderedList items -> Doc.OrderedList (fmap (fmap fromHaddock) items)
  Haddock.DocDefList defs -> Doc.DefList (fmap (Bifunctor.bimap fromHaddock fromHaddock) defs)
  Haddock.DocCodeBlock d -> Doc.CodeBlock (fromHaddock d)
  Haddock.DocHyperlink h ->
    Doc.Hyperlink
      Hyperlink.MkHyperlink
        { Hyperlink.url = Text.pack (Haddock.hyperlinkUrl h),
          Hyperlink.label = fmap fromHaddock (Haddock.hyperlinkLabel h)
        }
  Haddock.DocPic p ->
    Doc.Pic
      Picture.MkPicture
        { Picture.uri = Text.pack (Haddock.pictureUri p),
          Picture.title = fmap Text.pack (Haddock.pictureTitle p)
        }
  Haddock.DocMathInline s -> Doc.MathInline (Text.pack s)
  Haddock.DocMathDisplay s -> Doc.MathDisplay (Text.pack s)
  Haddock.DocAName s -> Doc.AName (Text.pack s)
  Haddock.DocProperty s -> Doc.Property (Text.pack s)
  Haddock.DocExamples es ->
    Doc.Examples
      ( fmap
          ( \e ->
              Example.MkExample
                { Example.expression = Text.pack (Haddock.exampleExpression e),
                  Example.result = fmap Text.pack (Haddock.exampleResult e)
                }
          )
          es
      )
  Haddock.DocHeader h ->
    Doc.Header
      Header.MkHeader
        { Header.level = Maybe.fromMaybe Level.One (Level.fromIntegral (Haddock.headerLevel h)),
          Header.title = fromHaddock (Haddock.headerTitle h)
        }
  Haddock.DocTable t ->
    Doc.Table
      Table.MkTable
        { Table.headerRows = fmap convertTableRow (Haddock.tableHeaderRows t),
          Table.bodyRows = fmap convertTableRow (Haddock.tableBodyRows t)
        }
    where
      convertTableRow :: Haddock.TableRow (Haddock.DocH Void.Void Identifier.Identifier) -> [TableCell.Cell Doc.Doc]
      convertTableRow row = fmap convertTableCell (Haddock.tableRowCells row)

      convertTableCell :: Haddock.TableCell (Haddock.DocH Void.Void Identifier.Identifier) -> TableCell.Cell Doc.Doc
      convertTableCell cell =
        TableCell.MkCell
          { TableCell.colspan = fromIntegral (Haddock.tableCellColspan cell),
            TableCell.rowspan = fromIntegral (Haddock.tableCellRowspan cell),
            TableCell.contents = fromHaddock (Haddock.tableCellContents cell)
          }

-- | Parse and convert documentation string to our Doc type.
parseDoc :: String -> Doc.Doc
parseDoc input =
  let metaDoc :: Haddock.MetaDoc Void.Void Haddock.Identifier
      metaDoc = Haddock.parseParas Nothing input
      haddockDoc :: Haddock.DocH Void.Void Haddock.Identifier
      haddockDoc = Haddock._doc metaDoc
      withIdentifiers :: Haddock.DocH Void.Void Identifier.Identifier
      withIdentifiers = Haddock.overIdentifier convertIdentifier haddockDoc
   in fromHaddock withIdentifiers
