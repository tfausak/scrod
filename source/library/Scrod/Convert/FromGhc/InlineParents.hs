-- | Resolve inline pragma parent relationships.
--
-- Associates inline signature items with their target declarations when
-- those declarations are defined in the same module. Uses the shared
-- parent association logic from 'Scrod.Convert.FromGhc.ParentAssociation'.
module Scrod.Convert.FromGhc.InlineParents where

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
associateInlineParents = ParentAssociation.associateParents
