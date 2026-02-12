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

-- | Convert export documentation.
convertExportDoc ::
  SrcLoc.GenLocated l (HsDoc.WithHsDocIdentifiers DocString.HsDocString Ghc.GhcPs) ->
  Doc.Doc
convertExportDoc lDoc =
  let hsDoc = SrcLoc.unLoc lDoc
      hsDocString = HsDoc.hsDocString hsDoc
      rendered = DocString.renderHsDocString hsDocString
   in parseDoc rendered

-- | Convert a located HsDoc to our 'Doc' type.
convertLHsDoc ::
  SrcLoc.GenLocated l (HsDoc.WithHsDocIdentifiers DocString.HsDocString Ghc.GhcPs) ->
  Doc.Doc
convertLHsDoc = convertExportDoc

-- | Parse documentation string to our 'Doc' type.
parseDoc :: String -> Doc.Doc
parseDoc input =
  let metaDoc :: Haddock.MetaDoc m Haddock.Identifier
      metaDoc = Haddock.parseParas Nothing input
      haddockDoc :: Haddock.DocH m Haddock.Identifier
      haddockDoc = Haddock._doc metaDoc
   in FromHaddock.fromHaddock haddockDoc

-- | Associate documentation comments with their target declarations.
associateDocs ::
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associateDocs decls =
  let withNextDocs = associateNextDocs decls
      withAllDocs = associatePrevDocs withNextDocs
   in withAllDocs

-- | Associate DocCommentNext with the following declaration.
associateNextDocs ::
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associateNextDocs = associateNextDocsLoop Doc.Empty

-- | Recursive helper for associating next-doc comments.
associateNextDocsLoop ::
  Doc.Doc ->
  [Syntax.LHsDecl Ghc.GhcPs] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associateNextDocsLoop _ [] = []
associateNextDocsLoop pendingDoc (lDecl : rest) = case SrcLoc.unLoc lDecl of
  Syntax.DocD _ (Hs.DocCommentNext lDoc) ->
    let newDoc = Internal.appendDoc pendingDoc $ convertLHsDoc lDoc
     in associateNextDocsLoop newDoc rest
  Syntax.DocD _ (Hs.DocCommentPrev _) ->
    (Doc.Empty, lDecl) : associateNextDocsLoop Doc.Empty rest
  _ ->
    (pendingDoc, lDecl) : associateNextDocsLoop Doc.Empty rest

-- | Associate DocCommentPrev with the preceding declaration.
associatePrevDocs ::
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associatePrevDocs = reverse . associatePrevDocsLoop . reverse

-- | Recursive helper for associating prev-doc comments.
associatePrevDocsLoop ::
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
associatePrevDocsLoop [] = []
associatePrevDocsLoop ((doc, lDecl) : rest) = case SrcLoc.unLoc lDecl of
  Syntax.DocD _ (Hs.DocCommentPrev lDoc) ->
    let prevDoc = convertLHsDoc lDoc
     in applyPrevDoc prevDoc $ associatePrevDocsLoop rest
  _ ->
    (doc, lDecl) : associatePrevDocsLoop rest

-- | Apply a prev-doc comment to the nearest preceding non-doc declaration.
applyPrevDoc ::
  Doc.Doc ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)] ->
  [(Doc.Doc, Syntax.LHsDecl Ghc.GhcPs)]
applyPrevDoc _ [] = []
applyPrevDoc prevDoc ((existingDoc, lDecl) : rest) = case SrcLoc.unLoc lDecl of
  Syntax.DocD {} -> (existingDoc, lDecl) : applyPrevDoc prevDoc rest
  _ -> (Internal.appendDoc existingDoc prevDoc, lDecl) : rest
