-- | Resolve fixity parent relationships.
--
-- Associates fixity signature items with their target declarations when
-- those declarations are defined in the same module. Uses the shared
-- parent association logic from 'Scrod.Convert.FromGhc.ParentAssociation'.
module Scrod.Convert.FromGhc.FixityParents where

import qualified Data.Set as Set
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Syntax as Syntax
import qualified Scrod.Convert.FromGhc.Internal as Internal
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
