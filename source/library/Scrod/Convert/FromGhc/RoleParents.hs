-- | Resolve role annotation parent relationships.
--
-- Associates role annotation items with their target type declarations
-- when those declarations are defined in the same module. Uses the shared
-- parent association logic from 'Scrod.Convert.FromGhc.ParentAssociation'.
module Scrod.Convert.FromGhc.RoleParents where

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
  Set.Set Location.Location ->
  [Located.Located Item.Item] ->
  [Located.Located Item.Item]
associateRoleParents = ParentAssociation.associateParents
