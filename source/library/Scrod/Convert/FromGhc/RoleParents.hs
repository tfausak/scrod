-- | Resolve role annotation parent relationships.
--
-- Associates role annotation items with their target type declarations
-- when those declarations are defined in the same module. Works like
-- 'Scrod.Convert.FromGhc.FixityParents' but for @type role@ declarations.
module Scrod.Convert.FromGhc.RoleParents where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Syntax as Syntax
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.Location as Location

-- | Extract the set of source locations that correspond to role annotation
-- declaration names.
extractRoleLocations ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Set.Set Location.Location
extractRoleLocations lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
   in Set.fromList $ concatMap extractDeclRoleLocations decls

-- | Extract role annotation name locations from a single declaration.
extractDeclRoleLocations ::
  Syntax.LHsDecl Ghc.GhcPs ->
  [Location.Location]
extractDeclRoleLocations lDecl = case SrcLoc.unLoc lDecl of
  Syntax.RoleAnnotD _ (Syntax.RoleAnnotDecl _ lName _) ->
    foldMap pure $ Internal.locationFromSrcSpan (Annotation.getLocA lName)
  _ -> []

-- | Associate role annotation items with their target declarations.
associateRoleParents ::
  Set.Set Location.Location ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
associateRoleParents roleLocations items =
  let nameToKey = buildNameToKeyMap roleLocations items
   in fmap (resolveRoleParent roleLocations nameToKey) items

-- | Build a map from item names to their keys, excluding role annotation items.
buildNameToKeyMap ::
  Set.Set Location.Location ->
  [Located.Located Item.Item] ->
  Map.Map ItemName.ItemName ItemKey.ItemKey
buildNameToKeyMap roleLocations =
  Map.fromList . concatMap getNameAndKey
  where
    getNameAndKey locItem =
      let val = Located.value locItem
       in case Item.name val of
            Nothing -> []
            Just name ->
              if Set.member (Located.location locItem) roleLocations
                then []
                else [(name, Item.key val)]

-- | Set the parentKey on a role annotation item by looking up the target name.
resolveRoleParent ::
  Set.Set Location.Location ->
  Map.Map ItemName.ItemName ItemKey.ItemKey ->
  Located.Located Item.Item ->
  Located.Located Item.Item
resolveRoleParent roleLocations nameToKey locItem =
  if Set.member (Located.location locItem) roleLocations
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
