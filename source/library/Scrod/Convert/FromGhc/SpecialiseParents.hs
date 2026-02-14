-- | Resolve specialise parent relationships.
--
-- Associates specialise signature items with their target declarations when
-- those declarations are defined in the same module. Uses the shared
-- parent association logic from 'Scrod.Convert.FromGhc.ParentAssociation'.
module Scrod.Convert.FromGhc.SpecialiseParents where

import qualified Data.Set as Set
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Syntax as Syntax
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Convert.FromGhc.ParentAssociation as ParentAssociation
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.Location as Location

-- | Extract the set of source locations that correspond to names inside
-- specialise signature declarations.
extractSpecialiseLocations ::
  SrcLoc.Located (Syntax.HsModule Ghc.GhcPs) ->
  Set.Set Location.Location
extractSpecialiseLocations lHsModule =
  let hsModule = SrcLoc.unLoc lHsModule
      decls = Syntax.hsmodDecls hsModule
   in Set.fromList $ concatMap extractDeclSpecialiseLocations decls

-- | Extract specialise name locations from a single declaration.
extractDeclSpecialiseLocations ::
  Syntax.LHsDecl Ghc.GhcPs ->
  [Location.Location]
extractDeclSpecialiseLocations lDecl = case SrcLoc.unLoc lDecl of
  Syntax.SigD _ (Syntax.SpecSig _ lName _ _) ->
    foldMap pure $ Internal.locationFromSrcSpan (Annotation.getLocA lName)
  Syntax.SigD _ (Syntax.SpecSigE _ _ lExpr _) ->
    extractExprNameLocations lExpr
  _ -> []

-- | Extract name locations from a SpecSigE expression.
extractExprNameLocations ::
  Syntax.LHsExpr Ghc.GhcPs ->
  [Location.Location]
extractExprNameLocations lExpr = case SrcLoc.unLoc lExpr of
  Syntax.ExprWithTySig _ body _ -> case SrcLoc.unLoc body of
    Syntax.HsVar _ lName ->
      foldMap pure . Internal.locationFromSrcSpan $ Annotation.getLocA lName
    _ -> []
  _ -> []

-- | Associate specialise items with their target declarations.
associateSpecialiseParents ::
  Set.Set Location.Location ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
associateSpecialiseParents = ParentAssociation.associateParents
