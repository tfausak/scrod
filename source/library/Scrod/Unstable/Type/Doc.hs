module Scrod.Unstable.Type.Doc where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Example as Example
import qualified Scrod.Unstable.Type.Header as Header
import qualified Scrod.Unstable.Type.Hyperlink as Hyperlink
import qualified Scrod.Unstable.Type.Identifier as Identifier
import qualified Scrod.Unstable.Type.Json as Json
import qualified Scrod.Unstable.Type.ModLink as ModLink
import qualified Scrod.Unstable.Type.Picture as Picture
import qualified Scrod.Unstable.Type.Table as Table

-- | Documentation AST, simplified from Haddock's 'DocH'.
--
-- Differences from 'Documentation.Haddock.Types.DocH':
--   - No 'mod' type parameter (was always 'Void' in Scrod)
--   - No 'id' type parameter (always 'Identifier')
--   - No 'DocIdentifierUnchecked' constructor (never used)
--   - Uses 'Text' instead of 'String'
data Doc
  = Empty
  | Append Doc Doc
  | String Text.Text
  | Paragraph Doc
  | Identifier Identifier.Identifier
  | Module (ModLink.ModLink Doc)
  | Emphasis Doc
  | Monospaced Doc
  | Bold Doc
  | UnorderedList [Doc]
  | OrderedList [(Int, Doc)]
  | DefList [(Doc, Doc)]
  | CodeBlock Doc
  | Hyperlink (Hyperlink.Hyperlink Doc)
  | Pic Picture.Picture
  | MathInline Text.Text
  | MathDisplay Text.Text
  | AName Text.Text
  | Property Text.Text
  | Examples [Example.Example]
  | Header (Header.Header Doc)
  | Table (Table.Table Doc)
  deriving (Eq, Ord, Show)

instance Semigroup Doc where
  x <> y = case (x, y) of
    (Empty, _) -> y
    (_, Empty) -> x
    _ -> Append x y

instance Monoid Doc where
  mempty = Empty

toJson :: Doc -> Json.Json
toJson doc = case doc of
  Empty -> Json.tag (Text.pack "Empty")
  Append d1 d2 -> Json.tagged (Text.pack "Append") $ Json.fromList [toJson d1, toJson d2]
  String t -> Json.tagged (Text.pack "String") $ Json.fromText t
  Paragraph d -> Json.tagged (Text.pack "Paragraph") $ toJson d
  Identifier i -> Json.tagged (Text.pack "Identifier") $ Identifier.toJson i
  Module m -> Json.tagged (Text.pack "Module") $ ModLink.toJson toJson m
  Emphasis d -> Json.tagged (Text.pack "Emphasis") $ toJson d
  Monospaced d -> Json.tagged (Text.pack "Monospaced") $ toJson d
  Bold d -> Json.tagged (Text.pack "Bold") $ toJson d
  UnorderedList ds -> Json.tagged (Text.pack "UnorderedList") . Json.fromList $ fmap toJson ds
  OrderedList items ->
    Json.tagged (Text.pack "OrderedList") . Json.fromList $
      fmap
        ( \(n, d) ->
            Json.object
              [ (Text.pack "number", Json.fromInt n),
                (Text.pack "doc", toJson d)
              ]
        )
        items
  DefList defs ->
    Json.tagged (Text.pack "DefList") . Json.fromList $
      fmap
        ( \(term, def) ->
            Json.object
              [ (Text.pack "term", toJson term),
                (Text.pack "definition", toJson def)
              ]
        )
        defs
  CodeBlock d -> Json.tagged (Text.pack "CodeBlock") $ toJson d
  Hyperlink h -> Json.tagged (Text.pack "Hyperlink") $ Hyperlink.toJson toJson h
  Pic p -> Json.tagged (Text.pack "Pic") $ Picture.toJson p
  MathInline t -> Json.tagged (Text.pack "MathInline") $ Json.fromText t
  MathDisplay t -> Json.tagged (Text.pack "MathDisplay") $ Json.fromText t
  AName t -> Json.tagged (Text.pack "AName") $ Json.fromText t
  Property t -> Json.tagged (Text.pack "Property") $ Json.fromText t
  Examples es -> Json.tagged (Text.pack "Examples") . Json.fromList $ fmap Example.toJson es
  Header h -> Json.tagged (Text.pack "Header") $ Header.toJson toJson h
  Table t -> Json.tagged (Text.pack "Table") $ Table.toJson toJson t
