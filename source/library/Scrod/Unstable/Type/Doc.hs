{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrod.Unstable.Type.Doc where

import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Scrod.Unstable.Type.Example as Example
import qualified Scrod.Unstable.Type.Header as Header
import qualified Scrod.Unstable.Type.Hyperlink as Hyperlink
import qualified Scrod.Unstable.Type.Identifier as Identifier
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers
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

append :: Doc -> Doc -> Doc
append x y = case (x, y) of
  (Empty, _) -> y
  (_, Empty) -> x
  _ -> Append x y

fromJson :: Aeson.Value -> Either String Doc
fromJson = \case
  Aeson.Object obj -> do
    tagJson <- JsonHelpers.lookupField obj "tag"
    tag <- case tagJson of
      Aeson.String t -> Right t
      _ -> Left "tag must be a string"
    case tag of
      "Empty" -> Right Empty
      "Append" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        case contentsJson of
          Aeson.Array vec | Vector.length vec == 2 -> do
            doc1 <- fromJson (vec Vector.! 0)
            doc2 <- fromJson (vec Vector.! 1)
            Right (Append doc1 doc2)
          _ -> Left "Append contents must be array of 2 elements"
      "String" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        txt <- case contentsJson of
          Aeson.String t -> Right t
          _ -> Left "String contents must be a string"
        Right (String txt)
      "Paragraph" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        doc <- fromJson contentsJson
        Right (Paragraph doc)
      "Identifier" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        ident <- Identifier.fromJson contentsJson
        Right (Identifier ident)
      "Module" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        modLink <- ModLink.fromJson fromJson contentsJson
        Right (Module modLink)
      "Emphasis" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        doc <- fromJson contentsJson
        Right (Emphasis doc)
      "Monospaced" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        doc <- fromJson contentsJson
        Right (Monospaced doc)
      "Bold" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        doc <- fromJson contentsJson
        Right (Bold doc)
      "UnorderedList" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        docs <- case contentsJson of
          Aeson.Array vec -> traverse fromJson (Vector.toList vec)
          _ -> Left "UnorderedList contents must be an array"
        Right (UnorderedList docs)
      "OrderedList" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        pairs <- case contentsJson of
          Aeson.Array vec -> traverse parsePair (Vector.toList vec)
          _ -> Left "OrderedList contents must be an array"
        Right (OrderedList pairs)
      "DefList" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        pairs <- case contentsJson of
          Aeson.Array vec -> traverse parseDefPair (Vector.toList vec)
          _ -> Left "DefList contents must be an array"
        Right (DefList pairs)
      "CodeBlock" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        doc <- fromJson contentsJson
        Right (CodeBlock doc)
      "Hyperlink" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        hyperlink <- Hyperlink.fromJson fromJson contentsJson
        Right (Hyperlink hyperlink)
      "Pic" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        pic <- Picture.fromJson contentsJson
        Right (Pic pic)
      "MathInline" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        txt <- case contentsJson of
          Aeson.String t -> Right t
          _ -> Left "MathInline contents must be a string"
        Right (MathInline txt)
      "MathDisplay" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        txt <- case contentsJson of
          Aeson.String t -> Right t
          _ -> Left "MathDisplay contents must be a string"
        Right (MathDisplay txt)
      "AName" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        txt <- case contentsJson of
          Aeson.String t -> Right t
          _ -> Left "AName contents must be a string"
        Right (AName txt)
      "Property" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        txt <- case contentsJson of
          Aeson.String t -> Right t
          _ -> Left "Property contents must be a string"
        Right (Property txt)
      "Examples" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        examples <- case contentsJson of
          Aeson.Array vec -> traverse Example.fromJson (Vector.toList vec)
          _ -> Left "Examples contents must be an array"
        Right (Examples examples)
      "Header" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        hdr <- Header.fromJson fromJson contentsJson
        Right (Header hdr)
      "Table" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        tbl <- Table.fromJson fromJson contentsJson
        Right (Table tbl)
      _ -> Left $ "unknown Doc tag: " <> Text.unpack tag
  _ -> Left "Doc must be an object"
  where
    parsePair :: Aeson.Value -> Either String (Int, Doc)
    parsePair = \case
      Aeson.Array vec | Vector.length vec == 2 -> do
        n <- case vec Vector.! 0 of
          Aeson.Number num -> case Scientific.floatingOrInteger num of
            Right i -> Right i
            Left (_ :: Double) -> Left "OrderedList index must be an integer"
          _ -> Left "OrderedList index must be a number"
        doc <- fromJson (vec Vector.! 1)
        Right (n, doc)
      _ -> Left "OrderedList pair must be array of 2 elements"
    parseDefPair :: Aeson.Value -> Either String (Doc, Doc)
    parseDefPair = \case
      Aeson.Array vec | Vector.length vec == 2 -> do
        term <- fromJson (vec Vector.! 0)
        def <- fromJson (vec Vector.! 1)
        Right (term, def)
      _ -> Left "DefList pair must be array of 2 elements"

toJson :: Doc -> Aeson.Value
toJson = \case
  Empty -> Aeson.object ["tag" Aeson..= ("Empty" :: Text.Text)]
  Append doc1 doc2 ->
    Aeson.object
      [ "tag" Aeson..= ("Append" :: Text.Text),
        "contents" Aeson..= [toJson doc1, toJson doc2]
      ]
  String txt ->
    Aeson.object
      [ "tag" Aeson..= ("String" :: Text.Text),
        "contents" Aeson..= txt
      ]
  Paragraph doc ->
    Aeson.object
      [ "tag" Aeson..= ("Paragraph" :: Text.Text),
        "contents" Aeson..= toJson doc
      ]
  Identifier ident ->
    Aeson.object
      [ "tag" Aeson..= ("Identifier" :: Text.Text),
        "contents" Aeson..= Identifier.toJson ident
      ]
  Module modLink ->
    Aeson.object
      [ "tag" Aeson..= ("Module" :: Text.Text),
        "contents" Aeson..= ModLink.toJson toJson modLink
      ]
  Emphasis doc ->
    Aeson.object
      [ "tag" Aeson..= ("Emphasis" :: Text.Text),
        "contents" Aeson..= toJson doc
      ]
  Monospaced doc ->
    Aeson.object
      [ "tag" Aeson..= ("Monospaced" :: Text.Text),
        "contents" Aeson..= toJson doc
      ]
  Bold doc ->
    Aeson.object
      [ "tag" Aeson..= ("Bold" :: Text.Text),
        "contents" Aeson..= toJson doc
      ]
  UnorderedList docs ->
    Aeson.object
      [ "tag" Aeson..= ("UnorderedList" :: Text.Text),
        "contents" Aeson..= fmap toJson docs
      ]
  OrderedList pairs ->
    Aeson.object
      [ "tag" Aeson..= ("OrderedList" :: Text.Text),
        "contents" Aeson..= fmap (\(n, doc) -> [Aeson.Number (fromIntegral n), toJson doc]) pairs
      ]
  DefList pairs ->
    Aeson.object
      [ "tag" Aeson..= ("DefList" :: Text.Text),
        "contents" Aeson..= fmap (\(term, def) -> [toJson term, toJson def]) pairs
      ]
  CodeBlock doc ->
    Aeson.object
      [ "tag" Aeson..= ("CodeBlock" :: Text.Text),
        "contents" Aeson..= toJson doc
      ]
  Hyperlink hyperlink ->
    Aeson.object
      [ "tag" Aeson..= ("Hyperlink" :: Text.Text),
        "contents" Aeson..= Hyperlink.toJson toJson hyperlink
      ]
  Pic pic ->
    Aeson.object
      [ "tag" Aeson..= ("Pic" :: Text.Text),
        "contents" Aeson..= Picture.toJson pic
      ]
  MathInline txt ->
    Aeson.object
      [ "tag" Aeson..= ("MathInline" :: Text.Text),
        "contents" Aeson..= txt
      ]
  MathDisplay txt ->
    Aeson.object
      [ "tag" Aeson..= ("MathDisplay" :: Text.Text),
        "contents" Aeson..= txt
      ]
  AName txt ->
    Aeson.object
      [ "tag" Aeson..= ("AName" :: Text.Text),
        "contents" Aeson..= txt
      ]
  Property txt ->
    Aeson.object
      [ "tag" Aeson..= ("Property" :: Text.Text),
        "contents" Aeson..= txt
      ]
  Examples examples ->
    Aeson.object
      [ "tag" Aeson..= ("Examples" :: Text.Text),
        "contents" Aeson..= fmap Example.toJson examples
      ]
  Header hdr ->
    Aeson.object
      [ "tag" Aeson..= ("Header" :: Text.Text),
        "contents" Aeson..= Header.toJson toJson hdr
      ]
  Table tbl ->
    Aeson.object
      [ "tag" Aeson..= ("Table" :: Text.Text),
        "contents" Aeson..= Table.toJson toJson tbl
      ]
