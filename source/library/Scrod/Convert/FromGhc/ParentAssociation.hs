-- | Shared logic for associating pragma items with their target declarations.
--
-- Multiple pragma types (fixity, inline, specialise, warning, role) follow
-- the same pattern: items at known pragma locations are parented to the
-- declaration with the matching name. This module provides a generic
-- implementation of that pattern.
module Scrod.Convert.FromGhc.ParentAssociation where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.Location as Location

-- | Associate pragma items with their target declarations by name.
--
-- Items whose locations appear in the given set are considered pragma items.
-- For each pragma item, we look up its name in a map built from the
-- remaining top-level items and set the parentKey accordingly.
associateParents ::
  Set.Set Location.Location ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
associateParents pragmaLocations items =
  let nameToKey = buildNameToKeyMap pragmaLocations items
   in fmap (resolveParent pragmaLocations nameToKey) items

-- | Build a map from item names to their keys, excluding pragma items
-- and child items. Only top-level declarations (those with no parentKey)
-- are eligible parents, so that @data T = T@ maps to the type
-- declaration rather than the constructor.
buildNameToKeyMap ::
  Set.Set Location.Location ->
  [Located.Located Item.Item] ->
  Map.Map ItemName.ItemName ItemKey.ItemKey
buildNameToKeyMap pragmaLocations =
  Map.fromList . concatMap getNameAndKey
  where
    getNameAndKey locItem =
      let val = Located.value locItem
       in case Item.name val of
            Nothing -> []
            Just name ->
              if Set.member (Located.location locItem) pragmaLocations
                || Maybe.isJust (Item.parentKey val)
                then []
                else [(name, Item.key val)]

-- | Set the parentKey on a pragma item by looking up its name.
resolveParent ::
  Set.Set Location.Location ->
  Map.Map ItemName.ItemName ItemKey.ItemKey ->
  Located.Located Item.Item ->
  Located.Located Item.Item
resolveParent pragmaLocations nameToKey locItem =
  if Set.member (Located.location locItem) pragmaLocations
    then case Item.name (Located.value locItem) of
      Nothing -> locItem
      Just name ->
        case Map.lookup name nameToKey of
          Nothing -> locItem
          Just parentKey ->
            locItem
              { Located.value =
                  (Located.value locItem) {Item.parentKey = Just parentKey}
              }
    else locItem
