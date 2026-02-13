-- | Merge items that share the same name.
--
-- When a declaration has both a type signature and a binding (or
-- multiple clauses), the GHC AST produces separate items for each.
-- This module merges them into a single item, combining their
-- documentation and signatures, and remaps child parent keys
-- accordingly.
module Scrod.Convert.FromGhc.Merge where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Located as Located

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
      combinedDoc = foldr (Internal.appendDoc . Item.documentation . Located.value) Doc.Empty sorted
      combinedSince =
        Maybe.listToMaybe . Maybe.mapMaybe (Item.since . Located.value) $ NonEmpty.toList sorted
      combinedSig =
        Maybe.listToMaybe . Maybe.mapMaybe (Item.signature . Located.value) $ NonEmpty.toList sorted
      mergedItem =
        (Located.value firstItem)
          { Item.documentation = combinedDoc,
            Item.since = combinedSince,
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
