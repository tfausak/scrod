-- | Merge standalone kind signatures into their declarations.
--
-- When a standalone kind signature (@type T :: ...@) and a matching
-- declaration (data, newtype, class, type synonym, type family, or
-- data family) share the same name, the kind signature is merged
-- into the declaration: the kind signature's text becomes the
-- declaration's signature, and their documentation and \@since\@
-- annotations are combined. The standalone kind signature item is
-- then removed from the output. This runs after merging so that
-- type signatures and bindings are merged first.
module Scrod.Convert.FromGhc.KindSigParents where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Located as Located

-- | Merge standalone kind signatures into their matching declarations.
associateKindSigParents ::
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
associateKindSigParents items =
  let kindSigMap = buildKindSigMap items
      declNames = buildDeclNameSet items
   in Maybe.mapMaybe (mergeOrRemoveKindSig kindSigMap declNames) items

-- | Build a map from standalone kind signature names to their items.
buildKindSigMap ::
  [Located.Located Item.Item] ->
  Map.Map ItemName.ItemName (Located.Located Item.Item)
buildKindSigMap =
  Map.fromList . Maybe.mapMaybe getKindSigEntry
  where
    getKindSigEntry locItem =
      let val = Located.value locItem
       in case (Item.kind val, Item.name val) of
            (ItemKind.StandaloneKindSig, Just name) ->
              Just (name, locItem)
            _ -> Nothing

-- | Collect names of top-level declarations that are not standalone
-- kind signatures. Used to decide whether a kind signature has a
-- matching declaration and should be consumed.
buildDeclNameSet ::
  [Located.Located Item.Item] ->
  Set.Set ItemName.ItemName
buildDeclNameSet =
  Set.fromList . Maybe.mapMaybe getDeclName
  where
    getDeclName locItem =
      let val = Located.value locItem
       in if Item.kind val /= ItemKind.StandaloneKindSig
            && Maybe.isNothing (Item.parentKey val)
            then Item.name val
            else Nothing

-- | For each item, either merge a matching kind signature into it,
-- remove a consumed kind signature, or pass it through unchanged.
mergeOrRemoveKindSig ::
  Map.Map ItemName.ItemName (Located.Located Item.Item) ->
  Set.Set ItemName.ItemName ->
  Located.Located Item.Item ->
  Maybe (Located.Located Item.Item)
mergeOrRemoveKindSig kindSigMap declNames locItem =
  let val = Located.value locItem
   in case Item.name val of
        Nothing -> Just locItem
        Just name
          | Item.kind val == ItemKind.StandaloneKindSig ->
              if Set.member name declNames
                then Nothing
                else Just locItem
          | Maybe.isJust (Item.parentKey val) ->
              Just locItem
          | otherwise ->
              case Map.lookup name kindSigMap of
                Nothing -> Just locItem
                Just kindSigItem ->
                  Just $ mergeKindSigInto kindSigItem locItem

-- | Merge a standalone kind signature's metadata into a declaration.
-- The kind signature's signature text and \@since\@ annotation take
-- precedence when present, and documentation is combined (kind
-- signature first, then declaration). The earlier source location of
-- the two items is used.
mergeKindSigInto ::
  Located.Located Item.Item ->
  Located.Located Item.Item ->
  Located.Located Item.Item
mergeKindSigInto kindSigItem declItem =
  let kindSigVal = Located.value kindSigItem
      declVal = Located.value declItem
      mergedSig = case Item.signature kindSigVal of
        Just s -> Just s
        Nothing -> Item.signature declVal
      mergedDoc =
        Internal.appendDoc
          (Item.documentation kindSigVal)
          (Item.documentation declVal)
      mergedSince =
        Internal.appendSince
          (Item.since kindSigVal)
          (Item.since declVal)
      mergedLocation =
        min (Located.location kindSigItem) (Located.location declItem)
   in declItem
        { Located.location = mergedLocation,
          Located.value =
            declVal
              { Item.signature = mergedSig,
                Item.documentation = mergedDoc,
                Item.since = mergedSince
              }
        }
