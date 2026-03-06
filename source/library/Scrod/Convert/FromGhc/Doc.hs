-- | Documentation comment association and doc string parsing.
--
-- Associates @DocCommentNext@ and @DocCommentPrev@ comments with their
-- target declarations, and converts Haddock doc strings into Scrod's
-- 'Doc.Doc' type via the Haddock parser.
module Scrod.Convert.FromGhc.Doc where

import qualified Data.Char as Char
import qualified Data.List as List
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
import qualified Scrod.Core.Definition as Definition
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

-- | Parse module documentation, extracting header fields as a definition list.
--
-- Haddock module comments may begin with metadata fields like
-- @Description@, @Copyright@, @License@, etc. This function strips
-- those fields from the input, parses the remaining text with
-- 'parseDoc', and prepends the fields as a 'Doc.DefList'.
parseModuleDoc :: String -> (Doc.Doc, Maybe Since.Since)
parseModuleDoc input =
  let (fields, rest) = parseModuleHeaderFields input
      (doc, since) = parseDoc rest
      fieldsDoc = case fields of
        [] -> Doc.Empty
        _ ->
          Doc.DefList $
            fmap
              ( \(name, value) ->
                  Definition.MkDefinition
                    { Definition.term = Doc.String $ Text.pack name,
                      Definition.definition = Doc.String . Text.pack $ trimValue value
                    }
              )
              fields
   in (Internal.appendDoc fieldsDoc doc, since)

-- | Trim leading and trailing whitespace from a field value.
trimValue :: String -> String
trimValue = List.dropWhileEnd Char.isSpace . dropWhile Char.isSpace

-- | Parse Haddock module header fields from a doc string.
--
-- Returns the list of @(fieldName, fieldValue)@ pairs and the
-- remaining doc string after the fields section.
parseModuleHeaderFields :: String -> ([(String, String)], String)
parseModuleHeaderFields input =
  let ls = lines input
      (blanks, nonBlanks) = span (all Char.isSpace) ls
      (fieldLines, restLines) = parseFieldLines nonBlanks
   in case fieldLines of
        [] -> ([], input)
        _ -> (fieldLines, unlines (blanks <> restLines))

-- | Known Haddock module header field names.
moduleHeaderFieldNames :: [String]
moduleHeaderFieldNames =
  [ "Module",
    "Description",
    "Copyright",
    "License",
    "Maintainer",
    "Stability",
    "Portability"
  ]

-- | Parse field lines from the beginning of a doc string.
--
-- Each field is @FieldName: value@ possibly followed by indented
-- continuation lines. Parsing stops at the first line that is not
-- a known field header or a continuation of the previous field.
parseFieldLines :: [String] -> ([(String, String)], [String])
parseFieldLines [] = ([], [])
parseFieldLines allLines@(l : rest) =
  case parseFieldHeader l of
    Just (name, value) ->
      let (continuations, remaining) = span isContinuationLine rest
          fullValue = List.intercalate "\n" (value : continuations)
          (moreFields, finalRest) = parseFieldLines remaining
       in ((name, fullValue) : moreFields, finalRest)
    Nothing -> ([], allLines)

-- | Try to parse a line as a field header (@FieldName: value@).
parseFieldHeader :: String -> Maybe (String, String)
parseFieldHeader line =
  let stripped = dropWhile Char.isSpace line
      lowered = fmap Char.toLower stripped
   in case List.find (\name -> fmap Char.toLower (name <> ":") `List.isPrefixOf` lowered) moduleHeaderFieldNames of
        Just name ->
          let after = drop (length name + 1) stripped
           in Just (name, after)
        Nothing -> Nothing

-- | A continuation line starts with whitespace and is not blank.
isContinuationLine :: String -> Bool
isContinuationLine [] = False
isContinuationLine (c : cs) = Char.isSpace c && not (all Char.isSpace cs)

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
