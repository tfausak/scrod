-- | Resolve warning parent relationships.
--
-- Associates warning pragma items with their target declarations when
-- those declarations are defined in the same module. Uses the shared
-- parent association logic from 'Scrod.Convert.FromGhc.ParentAssociation'.
module Scrod.Convert.FromGhc.WarningParents where

import qualified Data.Set as Set
import GHC.Hs.Decls ()
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Syntax as Syntax
import qualified Scrod.Convert.FromGhc.Internal as Internal
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
