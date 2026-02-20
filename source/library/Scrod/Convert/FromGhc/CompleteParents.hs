-- | Resolve COMPLETE pragma parent relationships.
--
-- Associates pattern synonym items with their COMPLETE pragma when
-- those patterns are listed in a @COMPLETE@ pragma in the same module.
-- Unlike other parent associations (where a pragma is parented to its
-- target), here the targets (pattern synonyms) are parented to the
-- pragma, so they are grouped under it in the output.
module Scrod.Convert.FromGhc.CompleteParents where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Syntax as Syntax
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.Location as Location

-- | Extract a mapping from pattern names referenced in COMPLETE pragmas
-- to the source location of the COMPLETE pragma that references them.
extractCompleteNames ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Map.Map ItemName.ItemName Location.Location
extractCompleteNames lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
   in Map.fromList $ concatMap extractDeclCompleteNames decls

-- | Extract pattern name to COMPLETE pragma location pairs from a
-- single declaration.
extractDeclCompleteNames ::
  Syntax.LHsDecl Ghc.GhcPs ->
  [(ItemName.ItemName, Location.Location)]
extractDeclCompleteNames lDecl = case SrcLoc.unLoc lDecl of
  Syntax.SigD _ (Syntax.CompleteMatchSig _ names _) ->
    case Internal.locationFromSrcSpan (Annotation.getLocA lDecl) of
      Nothing -> []
      Just loc -> fmap (\lName -> (Internal.extractIdPName lName, loc)) names
  _ -> []

-- | Associate pattern synonym items with their COMPLETE pragma parent.
associateCompleteParents ::
  Map.Map ItemName.ItemName Location.Location ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
associateCompleteParents completeNames items =
  let locationToKey = buildLocationToKeyMap items
   in fmap (resolveCompleteParent completeNames locationToKey) items

-- | Build a map from COMPLETE pragma item locations to their keys.
buildLocationToKeyMap ::
  [Located.Located Item.Item] ->
  Map.Map Location.Location ItemKey.ItemKey
buildLocationToKeyMap =
  Map.fromList . Maybe.mapMaybe getLocationAndKey
  where
    getLocationAndKey locItem =
      let val = Located.value locItem
       in case Item.kind val of
            ItemKind.CompletePragma ->
              Just (Located.location locItem, Item.key val)
            _ -> Nothing

-- | Set the parentKey on a pattern synonym item if its name appears
-- in a COMPLETE pragma.
resolveCompleteParent ::
  Map.Map ItemName.ItemName Location.Location ->
  Map.Map Location.Location ItemKey.ItemKey ->
  Located.Located Item.Item ->
  Located.Located Item.Item
resolveCompleteParent completeNames locationToKey locItem =
  let val = Located.value locItem
   in case Item.name val of
        Nothing -> locItem
        Just name ->
          if Maybe.isJust (Item.parentKey val)
            then locItem
            else case Map.lookup name completeNames of
              Nothing -> locItem
              Just loc -> case Map.lookup loc locationToKey of
                Nothing -> locItem
                Just parentKey ->
                  Internal.setParentKey parentKey locItem
