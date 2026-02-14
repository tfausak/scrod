-- | Resolve inline pragma parent relationships.
--
-- Associates inline signature items with their target declarations when
-- those declarations are defined in the same module. Works like
-- 'Scrod.Convert.FromGhc.FixityParents' but for @INLINE@, @NOINLINE@,
-- @INLINABLE@, and @OPAQUE@ pragmas.
module Scrod.Convert.FromGhc.InlineParents where

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

-- | Extract the set of source locations that correspond to names inside
-- inline signature declarations.
extractInlineLocations ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Set.Set Location.Location
extractInlineLocations lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
   in Set.fromList $ concatMap extractDeclInlineLocations decls

-- | Extract inline name locations from a single declaration.
extractDeclInlineLocations ::
  Syntax.LHsDecl Ghc.GhcPs ->
  [Location.Location]
extractDeclInlineLocations lDecl = case SrcLoc.unLoc lDecl of
  Syntax.SigD _ (Syntax.InlineSig _ lName _) ->
    foldMap pure $ Internal.locationFromSrcSpan (Annotation.getLocA lName)
  _ -> []

-- | Associate inline items with their target declarations.
associateInlineParents ::
  Set.Set Location.Location ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
associateInlineParents inlineLocations items =
  let nameToKey = buildNameToKeyMap inlineLocations items
   in fmap (resolveInlineParent inlineLocations nameToKey) items

-- | Build a map from item names to their keys, excluding inline items.
buildNameToKeyMap ::
  Set.Set Location.Location ->
  [Located.Located Item.Item] ->
  Map.Map ItemName.ItemName ItemKey.ItemKey
buildNameToKeyMap inlineLocations =
  Map.fromList . concatMap getNameAndKey
  where
    getNameAndKey locItem =
      let val = Located.value locItem
       in case Item.name val of
            Nothing -> []
            Just name ->
              if Set.member (Located.location locItem) inlineLocations
                then []
                else [(name, Item.key val)]

-- | Set the parentKey on an inline item by looking up the target name.
resolveInlineParent ::
  Set.Set Location.Location ->
  Map.Map ItemName.ItemName ItemKey.ItemKey ->
  Located.Located Item.Item ->
  Located.Located Item.Item
resolveInlineParent inlineLocations nameToKey locItem =
  if Set.member (Located.location locItem) inlineLocations
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
