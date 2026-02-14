-- | Resolve fixity parent relationships.
--
-- Associates fixity signature items with their target declarations when
-- those declarations are defined in the same module. Works like
-- 'Scrod.Convert.FromGhc.InstanceParents' but for @infixl@, @infixr@,
-- and @infix@ declarations.
module Scrod.Convert.FromGhc.FixityParents where

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
-- fixity signature declarations.
extractFixityLocations ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Set.Set Location.Location
extractFixityLocations lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
   in Set.fromList $ concatMap extractDeclFixityLocations decls

-- | Extract fixity name locations from a single declaration.
extractDeclFixityLocations ::
  Syntax.LHsDecl Ghc.GhcPs ->
  [Location.Location]
extractDeclFixityLocations lDecl = case SrcLoc.unLoc lDecl of
  Syntax.SigD _ (Syntax.FixSig _ (Syntax.FixitySig _ names _)) ->
    concatMap (foldMap pure . Internal.locationFromSrcSpan . Annotation.getLocA) names
  _ -> []

-- | Associate fixity items with their target declarations.
associateFixityParents ::
  Set.Set Location.Location ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
associateFixityParents fixityLocations items =
  let nameToKey = buildNameToKeyMap fixityLocations items
   in fmap (resolveFixityParent fixityLocations nameToKey) items

-- | Build a map from item names to their keys, excluding fixity
-- items and child items. Only top-level declarations (those with no
-- parentKey) are eligible parents, so that @data T = T@ maps to the
-- type declaration rather than the constructor.
buildNameToKeyMap ::
  Set.Set Location.Location ->
  [Located.Located Item.Item] ->
  Map.Map ItemName.ItemName ItemKey.ItemKey
buildNameToKeyMap fixityLocations =
  Map.fromList . concatMap getNameAndKey
  where
    getNameAndKey locItem =
      let val = Located.value locItem
       in case Item.name val of
            Nothing -> []
            Just name ->
              if Set.member (Located.location locItem) fixityLocations
                || Maybe.isJust (Item.parentKey val)
                then []
                else [(name, Item.key val)]

-- | Set the parentKey on a fixity item by looking up the target name.
resolveFixityParent ::
  Set.Set Location.Location ->
  Map.Map ItemName.ItemName ItemKey.ItemKey ->
  Located.Located Item.Item ->
  Located.Located Item.Item
resolveFixityParent fixityLocations nameToKey locItem =
  if Set.member (Located.location locItem) fixityLocations
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
