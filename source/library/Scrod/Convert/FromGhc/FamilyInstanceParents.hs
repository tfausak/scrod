-- | Resolve family instance parent relationships.
--
-- Associates type family instance and data family instance items with
-- their corresponding family declarations when both are defined in the
-- same module.
module Scrod.Convert.FromGhc.FamilyInstanceParents where

import qualified Data.Map as Map
import GHC.Hs ()
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

-- | Extract a map from source locations of family instance declarations
-- to the family name they reference.
extractFamilyInstanceNames ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Map.Map Location.Location ItemName.ItemName
extractFamilyInstanceNames lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
   in Map.fromList $ concatMap extractDeclFamilyInstanceName decls

-- | Extract family instance name from a single declaration.
extractDeclFamilyInstanceName ::
  Syntax.LHsDecl Ghc.GhcPs ->
  [(Location.Location, ItemName.ItemName)]
extractDeclFamilyInstanceName lDecl = case SrcLoc.unLoc lDecl of
  Syntax.InstD _ (Syntax.TyFamInstD _ tyFamInst) ->
    let eqn = Syntax.tfid_eqn tyFamInst
        familyName = Internal.extractIdPName $ Syntax.feqn_tycon eqn
     in foldMap (\loc -> [(loc, familyName)]) $
          Internal.locationFromSrcSpan (Annotation.getLocA lDecl)
  Syntax.InstD _ (Syntax.DataFamInstD _ dataFamInst) ->
    let eqn = Syntax.dfid_eqn dataFamInst
        familyName = Internal.extractIdPName $ Syntax.feqn_tycon eqn
     in foldMap (\loc -> [(loc, familyName)]) $
          Internal.locationFromSrcSpan (Annotation.getLocA lDecl)
  _ -> []

-- | Associate family instance items with their family declarations.
associateFamilyInstanceParents ::
  Map.Map Location.Location ItemName.ItemName ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
associateFamilyInstanceParents familyInstanceNames items =
  let familyNameToKey = buildFamilyNameToKeyMap items
   in fmap (resolveFamilyInstanceParent familyInstanceNames familyNameToKey) items

-- | Build a map from family names to their keys.
buildFamilyNameToKeyMap ::
  [Located.Located Item.Item] ->
  Map.Map ItemName.ItemName ItemKey.ItemKey
buildFamilyNameToKeyMap =
  Map.fromList . concatMap getFamilyNameAndKey
  where
    getFamilyNameAndKey locItem =
      let val = Located.value locItem
       in case Item.name val of
            Nothing -> []
            Just name ->
              if isFamilyKind (Item.kind val)
                then [(name, Item.key val)]
                else []

-- | Check if an item kind represents a family declaration.
isFamilyKind :: ItemKind.ItemKind -> Bool
isFamilyKind k = case k of
  ItemKind.OpenTypeFamily -> True
  ItemKind.DataFamily -> True
  _ -> False

-- | Set the parentKey on a family instance item by looking up the family name.
resolveFamilyInstanceParent ::
  Map.Map Location.Location ItemName.ItemName ->
  Map.Map ItemName.ItemName ItemKey.ItemKey ->
  Located.Located Item.Item ->
  Located.Located Item.Item
resolveFamilyInstanceParent familyInstanceNames familyNameToKey locItem =
  case Map.lookup (Located.location locItem) familyInstanceNames of
    Nothing -> locItem
    Just familyName ->
      case Map.lookup familyName familyNameToKey of
        Nothing -> locItem
        Just parentKey ->
          locItem
            { Located.value =
                (Located.value locItem) {Item.parentKey = Just parentKey}
            }
