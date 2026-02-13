-- | Documentation comment association and doc string parsing.
--
-- Associates @DocCommentNext@ and @DocCommentPrev@ comments with their
-- target declarations, and converts Haddock doc strings into Scrod's
-- 'Doc.Doc' type via the Haddock parser.
module Scrod.Convert.FromGhc.Doc where

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
associateDocs ::
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)]
associateDocs decls =
  let withNextDocs = associateNextDocs decls
      withAllDocs = associatePrevDocs withNextDocs
   in withAllDocs

-- | Associate DocCommentNext with the following declaration.
associateNextDocs ::
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)]
associateNextDocs = associateNextDocsLoop Doc.Empty Nothing

-- | Recursive helper for associating next-doc comments.
associateNextDocsLoop ::
  Doc.Doc ->
  Maybe Since.Since ->
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)]
associateNextDocsLoop _ _ [] = []
associateNextDocsLoop pendingDoc pendingSince (lDecl : rest) = case SrcLoc.unLoc lDecl of
  Syntax.DocD _ (Hs.DocCommentNext lDoc) ->
    let (newDoc, newSince) = convertLHsDoc lDoc
     in associateNextDocsLoop (Internal.appendDoc pendingDoc newDoc) (Internal.appendSince pendingSince newSince) rest
  Syntax.DocD _ (Hs.DocCommentPrev _) ->
    (Doc.Empty, Nothing, lDecl) : associateNextDocsLoop Doc.Empty Nothing rest
  _ ->
    (pendingDoc, pendingSince, lDecl) : associateNextDocsLoop Doc.Empty Nothing rest

-- | Associate DocCommentPrev with the preceding declaration.
associatePrevDocs ::
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)] ->
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)]
associatePrevDocs = reverse . associatePrevDocsLoop . reverse

-- | Recursive helper for associating prev-doc comments.
associatePrevDocsLoop ::
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)] ->
  [(Doc.Doc, Maybe Since.Since, Syntax.LHsDecl Ghc.GhcPs)]
associatePrevDocsLoop [] = []
associatePrevDocsLoop ((doc, docSince, lDecl) : rest) = case SrcLoc.unLoc lDecl of
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
applyPrevDoc _ _ [] = []
applyPrevDoc prevDoc prevSince ((existingDoc, existingSince, lDecl) : rest) = case SrcLoc.unLoc lDecl of
  Syntax.DocD {} -> (existingDoc, existingSince, lDecl) : applyPrevDoc prevDoc prevSince rest
  _ -> (Internal.appendDoc existingDoc prevDoc, Internal.appendSince existingSince prevSince, lDecl) : rest
