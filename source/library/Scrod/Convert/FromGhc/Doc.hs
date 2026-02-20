-- | Documentation comment association and doc string parsing.
--
-- Associates @DocCommentNext@ and @DocCommentPrev@ comments with their
-- target declarations, and converts Haddock doc strings into Scrod's
-- 'Doc.Doc' type via the Haddock parser.
module Scrod.Convert.FromGhc.Doc where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified GHC.Hs as Hs
import qualified GHC.Hs.Doc as HsDoc
import qualified GHC.Hs.DocString as DocString
import qualified GHC.Hs.Extension as Ghc
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Syntax as Syntax
import qualified Scrod.Convert.FromGhc.Internal as Internal
import qualified Scrod.Convert.FromHaddock as FromHaddock
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Since as Since

-- | Convert export documentation (doc only, discards @since).
convertExportDoc ::
  SrcLoc.GenLocated l (HsDoc.WithHsDocIdentifiers DocString.HsDocString Ghc.GhcPs) ->
  Doc.Doc
convertExportDoc = fst . convertLHsDoc

-- | Convert a located HsDoc to our 'Doc' type and optional @since.
convertLHsDoc ::
  SrcLoc.GenLocated l (HsDoc.WithHsDocIdentifiers DocString.HsDocString Ghc.GhcPs) ->
  (Doc.Doc, Maybe Since.Since)
convertLHsDoc lDoc =
  let hsDoc = SrcLoc.unLoc lDoc
      hsDocString = HsDoc.hsDocString hsDoc
      rendered = DocString.renderHsDocString hsDocString
   in parseDoc rendered

-- | Parse documentation string to our 'Doc' type and optional @since.
parseDoc :: String -> (Doc.Doc, Maybe Since.Since)
parseDoc input =
  let metaDoc :: Haddock.MetaDoc m Haddock.Identifier
      metaDoc = Haddock.parseParas Nothing input
      doc = FromHaddock.fromHaddock $ Haddock._doc metaDoc
      itemSince = Haddock._metaSince (Haddock._meta metaDoc) >>= Internal.metaSinceToSince
   in (doc, itemSince)

-- | Associate documentation comments with their target declarations.
--
-- Named doc chunks whose name appears in @referencedChunkNames@ (i.e. they
-- are referenced in the export list) are skipped so they don't create items.
-- Unreferenced named chunks pass through as declarations so they become
-- top-level items.
associateDocs ::
  Set.Set Text.Text ->
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)]
associateDocs referencedChunkNames decls =
  let withNextDocs = associateNextDocs referencedChunkNames decls
      withAllDocs = associatePrevDocs withNextDocs
   in withAllDocs

-- | Associate DocCommentNext with the following declaration.
associateNextDocs ::
  Set.Set Text.Text ->
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)]
associateNextDocs referencedChunkNames = associateNextDocsLoop referencedChunkNames Doc.Empty Nothing

-- | Recursive helper for associating next-doc comments.
associateNextDocsLoop ::
  Set.Set Text.Text ->
  Doc.Doc ->
  Maybe Since.Since ->
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)]
associateNextDocsLoop referencedChunkNames pendingDoc pendingSince decls = case decls of
  [] -> []
  lDecl : rest -> case SrcLoc.unLoc lDecl of
    Syntax.DocD _ (Hs.DocCommentNext lDoc) ->
      let (newDoc, newSince) = convertLHsDoc lDoc
       in associateNextDocsLoop referencedChunkNames (Internal.appendDoc pendingDoc newDoc) (Internal.appendSince pendingSince newSince) rest
    Syntax.DocD _ (Hs.DocCommentPrev _) ->
      (Doc.Empty, Nothing, lDecl) : associateNextDocsLoop referencedChunkNames Doc.Empty Nothing rest
    Syntax.DocD _ (Hs.DocCommentNamed name _)
      | Set.member (Text.pack name) referencedChunkNames ->
          associateNextDocsLoop referencedChunkNames Doc.Empty Nothing rest
      | otherwise ->
          (Doc.Empty, Nothing, lDecl) : associateNextDocsLoop referencedChunkNames Doc.Empty Nothing rest
    _ ->
      (pendingDoc, pendingSince, lDecl) : associateNextDocsLoop referencedChunkNames Doc.Empty Nothing rest

-- | Associate DocCommentPrev with the preceding declaration.
associatePrevDocs ::
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)] ->
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)]
associatePrevDocs = reverse . associatePrevDocsLoop . reverse

-- | Recursive helper for associating prev-doc comments.
associatePrevDocsLoop ::
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)] ->
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)]
associatePrevDocsLoop triples = case triples of
  [] -> []
  (doc, docSince, lDecl) : rest -> case SrcLoc.unLoc lDecl of
    Syntax.DocD _ (Hs.DocCommentPrev lDoc) ->
      let (prevDoc, prevSince) = convertLHsDoc lDoc
       in applyPrevDoc prevDoc prevSince $ associatePrevDocsLoop rest
    _ ->
      (doc, docSince, lDecl) : associatePrevDocsLoop rest

-- | Apply a prev-doc comment to the nearest preceding non-doc declaration.
applyPrevDoc ::
  Doc.Doc ->
  Maybe Since.Since ->
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)] ->
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)]
applyPrevDoc prevDoc prevSince triples = case triples of
  [] -> []
  (existingDoc, existingSince, lDecl) : rest -> case SrcLoc.unLoc lDecl of
    Syntax.DocD {} -> (existingDoc, existingSince, lDecl) : applyPrevDoc prevDoc prevSince rest
    _ -> (Internal.appendDoc existingDoc prevDoc, Internal.appendSince existingSince prevSince, lDecl) : rest
