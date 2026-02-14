-- | Resolve instance parent relationships.
--
-- Associates class instances and standalone deriving declarations with
-- their parent types or classes when those are defined in the same
-- module. For @instance C T@, if @T@ is defined locally, the instance
-- is parented under @T@. Otherwise, if @C@ is defined locally, the
-- instance is parented under @C@.
module Scrod.Convert.FromGhc.InstanceParents where

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

-- | Extract the head type name for each instance or standalone deriving
-- declaration, keyed by source location.
extractInstanceHeadTypeNames ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Map.Map Location.Location ItemName.ItemName
extractInstanceHeadTypeNames lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
   in Map.fromList $ Maybe.mapMaybe extractDeclInstanceHeadType decls

-- | Extract the class name for each instance or standalone deriving
-- declaration, keyed by source location.
extractInstanceClassNames ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Map.Map Location.Location ItemName.ItemName
extractInstanceClassNames lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
   in Map.fromList $ Maybe.mapMaybe extractDeclInstanceClass decls

-- | Extract the class name from a single declaration, if it is an
-- instance or standalone deriving declaration.
extractDeclInstanceClass ::
  Syntax.LHsDecl Ghc.GhcPs ->
  Maybe (Location.Location, ItemName.ItemName)
extractDeclInstanceClass lDecl = case SrcLoc.unLoc lDecl of
  Syntax.InstD _ inst -> case inst of
    Syntax.ClsInstD _ clsInst -> do
      location <- Internal.locationFromSrcSpan (Annotation.getLocA lDecl)
      className <- extractClassName . Syntax.sig_body . SrcLoc.unLoc $ Syntax.cid_poly_ty clsInst
      Just (location, className)
    _ -> Nothing
  Syntax.DerivD _ derivDecl -> do
    location <- Internal.locationFromSrcSpan (Annotation.getLocA lDecl)
    className <- extractClassName . Syntax.sig_body . SrcLoc.unLoc . Syntax.hswc_body $ Syntax.deriv_type derivDecl
    Just (location, className)
  _ -> Nothing

-- | Extract the head type name from a single declaration, if it is an
-- instance or standalone deriving declaration.
extractDeclInstanceHeadType ::
  Syntax.LHsDecl Ghc.GhcPs ->
  Maybe (Location.Location, ItemName.ItemName)
extractDeclInstanceHeadType lDecl = case SrcLoc.unLoc lDecl of
  Syntax.InstD _ inst -> case inst of
    Syntax.ClsInstD _ clsInst -> do
      location <- Internal.locationFromSrcSpan (Annotation.getLocA lDecl)
      headType <- extractHeadTypeName . Syntax.sig_body . SrcLoc.unLoc $ Syntax.cid_poly_ty clsInst
      Just (location, headType)
    _ -> Nothing
  Syntax.DerivD _ derivDecl -> do
    location <- Internal.locationFromSrcSpan (Annotation.getLocA lDecl)
    headType <- extractHeadTypeName . Syntax.sig_body . SrcLoc.unLoc . Syntax.hswc_body $ Syntax.deriv_type derivDecl
    Just (location, headType)
  _ -> Nothing

-- | Extract the head type constructor name from the last argument of a
-- type application. For @C T@ this returns @T@; for @C (Maybe a)@ this
-- returns @Maybe@.
extractHeadTypeName :: Syntax.LHsType Ghc.GhcPs -> Maybe ItemName.ItemName
extractHeadTypeName lTy = case SrcLoc.unLoc lTy of
  Syntax.HsAppTy _ _ arg -> extractOutermostTyCon arg
  Syntax.HsQualTy _ _ body -> extractHeadTypeName body
  Syntax.HsForAllTy _ _ body -> extractHeadTypeName body
  Syntax.HsParTy _ inner -> extractHeadTypeName inner
  _ -> Nothing

-- | Extract the class name from an instance head. For @C T@ this
-- returns @C@; for @Functor F@ this returns @Functor@. For nullary
-- classes (e.g., @instance C@) this returns @C@.
extractClassName :: Syntax.LHsType Ghc.GhcPs -> Maybe ItemName.ItemName
extractClassName lTy = case SrcLoc.unLoc lTy of
  Syntax.HsAppTy _ fun _ -> extractOutermostTyCon fun
  Syntax.HsAppKindTy _ fun _ -> extractOutermostTyCon fun
  Syntax.HsTyVar {} -> extractOutermostTyCon lTy
  Syntax.HsQualTy _ _ body -> extractClassName body
  Syntax.HsForAllTy _ _ body -> extractClassName body
  Syntax.HsParTy _ inner -> extractClassName inner
  _ -> Nothing

-- | Extract the outermost type constructor name from a type. For @T@
-- this returns @T@; for @Maybe a@ this returns @Maybe@.
extractOutermostTyCon :: Syntax.LHsType Ghc.GhcPs -> Maybe ItemName.ItemName
extractOutermostTyCon lTy = case SrcLoc.unLoc lTy of
  Syntax.HsTyVar _ _ lName -> Just . ItemName.MkItemName $ Internal.extractRdrName lName
  Syntax.HsAppTy _ fun _ -> extractOutermostTyCon fun
  Syntax.HsAppKindTy _ fun _ -> extractOutermostTyCon fun
  Syntax.HsParTy _ inner -> extractOutermostTyCon inner
  _ -> Nothing

-- | Associate instances and standalone deriving declarations with their
-- parent types or classes when those are defined in the same module.
associateInstanceParents ::
  Map.Map Location.Location ItemName.ItemName ->
  Map.Map Location.Location ItemName.ItemName ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
associateInstanceParents headTypeNames classNames items =
  let typeNameToKey = buildTypeNameToKeyMap items
   in fmap (resolveInstanceParent headTypeNames classNames typeNameToKey) items

-- | Build a map from type/class names to their item keys.
buildTypeNameToKeyMap ::
  [Located.Located Item.Item] ->
  Map.Map ItemName.ItemName ItemKey.ItemKey
buildTypeNameToKeyMap =
  Map.fromList . Maybe.mapMaybe getTypeNameAndKey
  where
    getTypeNameAndKey locItem =
      let val = Located.value locItem
       in case Item.parentKey val of
            Just _ -> Nothing
            Nothing ->
              if isTypeOrClassKind (Item.kind val)
                then fmap (\n -> (n, Item.key val)) (Item.name val)
                else Nothing

-- | Check if an item kind represents a type or class definition.
isTypeOrClassKind :: ItemKind.ItemKind -> Bool
isTypeOrClassKind kind = case kind of
  ItemKind.DataType -> True
  ItemKind.Newtype -> True
  ItemKind.TypeData -> True
  ItemKind.TypeSynonym -> True
  ItemKind.Class -> True
  _ -> False

-- | Try to resolve an instance's parent from the head type maps.
-- First tries the head type name (e.g., @T@ from @instance C T@),
-- then falls back to the class name (e.g., @C@).
resolveInstanceParent ::
  Map.Map Location.Location ItemName.ItemName ->
  Map.Map Location.Location ItemName.ItemName ->
  Map.Map ItemName.ItemName ItemKey.ItemKey ->
  Located.Located Item.Item ->
  Located.Located Item.Item
resolveInstanceParent headTypeNames classNames typeNameToKey locItem =
  let val = Located.value locItem
   in case Item.parentKey val of
        Just _ -> locItem
        Nothing ->
          if Item.kind val == ItemKind.ClassInstance || Item.kind val == ItemKind.StandaloneDeriving
            then case lookupParentKey (Located.location locItem) headTypeNames typeNameToKey of
              Just parentKey ->
                locItem {Located.value = val {Item.parentKey = Just parentKey}}
              Nothing -> case lookupParentKey (Located.location locItem) classNames typeNameToKey of
                Just parentKey ->
                  locItem {Located.value = val {Item.parentKey = Just parentKey}}
                Nothing -> locItem
            else locItem

-- | Look up a parent key by first resolving a location to a name,
-- then resolving that name to a key.
lookupParentKey ::
  Location.Location ->
  Map.Map Location.Location ItemName.ItemName ->
  Map.Map ItemName.ItemName ItemKey.ItemKey ->
  Maybe ItemKey.ItemKey
lookupParentKey location nameMap keyMap = do
  name <- Map.lookup location nameMap
  Map.lookup name keyMap
