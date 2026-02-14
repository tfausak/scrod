-- | Resolve standalone kind signature parent relationships.
--
-- Associates type, data, newtype, and class declarations with their
-- corresponding standalone kind signature when both are defined in
-- the same module. This runs after merging so that type signatures
-- and bindings are merged first.
module Scrod.Convert.FromGhc.KindSigParents where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Located as Located

-- | Associate declarations with their standalone kind signatures.
associateKindSigParents ::
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
associateKindSigParents items =
  let kindSigNameToKey = buildKindSigNameToKeyMap items
   in fmap (resolveKindSigParent kindSigNameToKey) items

-- | Build a map from standalone kind signature names to their keys.
buildKindSigNameToKeyMap ::
  [Located.Located Item.Item] ->
  Map.Map ItemName.ItemName ItemKey.ItemKey
buildKindSigNameToKeyMap =
  Map.fromList . Maybe.mapMaybe getKindSigNameAndKey
  where
    getKindSigNameAndKey locItem =
      let val = Located.value locItem
       in case (Item.kind val, Item.name val) of
            (ItemKind.StandaloneKindSig, Just name) ->
              Just (name, Item.key val)
            _ -> Nothing

-- | Set the parentKey on a declaration item if a standalone kind
-- signature with the same name exists.
resolveKindSigParent ::
  Map.Map ItemName.ItemName ItemKey.ItemKey ->
  Located.Located Item.Item ->
  Located.Located Item.Item
resolveKindSigParent kindSigNameToKey locItem =
  let val = Located.value locItem
   in case Item.name val of
        Nothing -> locItem
        Just name ->
          if Maybe.isJust (Item.parentKey val)
            || Item.kind val == ItemKind.StandaloneKindSig
            then locItem
            else case Map.lookup name kindSigNameToKey of
              Nothing -> locItem
              Just parentKey ->
                locItem
                  { Located.value =
                      val {Item.parentKey = Just parentKey}
                  }
