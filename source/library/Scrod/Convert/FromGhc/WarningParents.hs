-- TODO: Figure out why this is necessary and remove it.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- | Resolve warning parent relationships.
--
-- Associates warning pragma items with their target declarations when
-- those declarations are defined in the same module. Works like
-- 'Scrod.Convert.FromGhc.InstanceParents' but for @{-\# WARNING \#-}@
-- and @{-\# DEPRECATED \#-}@ pragmas.
module Scrod.Convert.FromGhc.WarningParents where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
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

-- | Extract the set of source locations that correspond to names inside
-- warning\/deprecated pragma declarations.
extractWarningLocations ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Set.Set Location.Location
extractWarningLocations lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
   in Set.fromList $ concatMap extractDeclWarningLocations decls

-- | Extract warning name locations from a single declaration.
extractDeclWarningLocations ::
  Syntax.LHsDecl Ghc.GhcPs ->
  [Location.Location]
extractDeclWarningLocations lDecl = case SrcLoc.unLoc lDecl of
  Syntax.WarningD _ (Syntax.Warnings _ warnDecls) ->
    concatMap extractWarnDeclLocations warnDecls
  _ -> []

-- | Extract name locations from a single warning declaration.
extractWarnDeclLocations ::
  Syntax.LWarnDecl Ghc.GhcPs ->
  [Location.Location]
extractWarnDeclLocations lWarnDecl = case SrcLoc.unLoc lWarnDecl of
  Syntax.Warning _ names _ ->
    concatMap (foldMap pure . Internal.locationFromSrcSpan . Annotation.getLocA) names
  Syntax.XWarnDecl {} -> []

-- | Associate warning items with their target declarations.
associateWarningParents ::
  Set.Set Location.Location ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
associateWarningParents warningLocations items =
  let nameToKey = buildNameToKeyMap warningLocations items
   in fmap (resolveWarningParent warningLocations nameToKey) items

-- | Build a map from item names to their keys, excluding warning items.
buildNameToKeyMap ::
  Set.Set Location.Location ->
  [Located.Located Item.Item] ->
  Map.Map ItemName.ItemName ItemKey.ItemKey
buildNameToKeyMap warningLocations =
  Map.fromList . concatMap getNameAndKey
  where
    getNameAndKey locItem =
      let val = Located.value locItem
       in case Item.name val of
            Nothing -> []
            Just name ->
              if Set.member (Located.location locItem) warningLocations
                then []
                else
                  if Maybe.isNothing (Item.parentKey val)
                    then [(name, Item.key val)]
                    else []

-- | Set the parentKey on a warning item by looking up the target name.
resolveWarningParent ::
  Set.Set Location.Location ->
  Map.Map ItemName.ItemName ItemKey.ItemKey ->
  Located.Located Item.Item ->
  Located.Located Item.Item
resolveWarningParent warningLocations nameToKey locItem =
  if Set.member (Located.location locItem) warningLocations
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
