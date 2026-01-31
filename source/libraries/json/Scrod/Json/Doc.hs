{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Doc where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Scrod.Json.Example as Example
import qualified Scrod.Json.Header as Header
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.Hyperlink as Hyperlink
import qualified Scrod.Json.Identifier as Identifier
import qualified Scrod.Json.ModLink as ModLink
import qualified Scrod.Json.Picture as Picture
import qualified Scrod.Json.Table as Table
import qualified Scrod.Type.Doc as Type

fromJson :: Aeson.Value -> Either String Type.Doc
fromJson value = case value of
  Aeson.Object obj -> do
    tagJson <- Helpers.lookupField obj "tag"
    tag <- case tagJson of
      Aeson.String t -> Right t
      _ -> Left "tag must be a string"
    case tag of
      "Empty" -> Right Type.Empty
      "Append" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        case contentsJson of
          Aeson.Array vec | Vector.length vec == 2 -> do
            doc1 <- fromJson (vec Vector.! 0)
            doc2 <- fromJson (vec Vector.! 1)
            Right (Type.Append doc1 doc2)
          _ -> Left "Append contents must be array of 2 elements"
      "String" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        txt <- case contentsJson of
          Aeson.String t -> Right t
          _ -> Left "String contents must be a string"
        Right (Type.String txt)
      "Paragraph" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        doc <- fromJson contentsJson
        Right (Type.Paragraph doc)
      "Identifier" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        ident <- Identifier.fromJson contentsJson
        Right (Type.Identifier ident)
      "Module" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        modLink <- ModLink.fromJson fromJson contentsJson
        Right (Type.Module modLink)
      "Emphasis" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        doc <- fromJson contentsJson
        Right (Type.Emphasis doc)
      "Monospaced" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        doc <- fromJson contentsJson
        Right (Type.Monospaced doc)
      "Bold" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        doc <- fromJson contentsJson
        Right (Type.Bold doc)
      "UnorderedList" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        docs <- case contentsJson of
          Aeson.Array vec -> traverse fromJson (Vector.toList vec)
          _ -> Left "UnorderedList contents must be an array"
        Right (Type.UnorderedList docs)
      "OrderedList" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        pairs <- case contentsJson of
          Aeson.Array vec -> traverse parsePair (Vector.toList vec)
          _ -> Left "OrderedList contents must be an array"
        Right (Type.OrderedList pairs)
      "DefList" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        pairs <- case contentsJson of
          Aeson.Array vec -> traverse parseDefPair (Vector.toList vec)
          _ -> Left "DefList contents must be an array"
        Right (Type.DefList pairs)
      "CodeBlock" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        doc <- fromJson contentsJson
        Right (Type.CodeBlock doc)
      "Hyperlink" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        hyperlink <- Hyperlink.fromJson fromJson contentsJson
        Right (Type.Hyperlink hyperlink)
      "Pic" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        pic <- Picture.fromJson contentsJson
        Right (Type.Pic pic)
      "MathInline" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        txt <- case contentsJson of
          Aeson.String t -> Right t
          _ -> Left "MathInline contents must be a string"
        Right (Type.MathInline txt)
      "MathDisplay" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        txt <- case contentsJson of
          Aeson.String t -> Right t
          _ -> Left "MathDisplay contents must be a string"
        Right (Type.MathDisplay txt)
      "AName" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        txt <- case contentsJson of
          Aeson.String t -> Right t
          _ -> Left "AName contents must be a string"
        Right (Type.AName txt)
      "Property" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        txt <- case contentsJson of
          Aeson.String t -> Right t
          _ -> Left "Property contents must be a string"
        Right (Type.Property txt)
      "Examples" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        examples <- case contentsJson of
          Aeson.Array vec -> traverse Example.fromJson (Vector.toList vec)
          _ -> Left "Examples contents must be an array"
        Right (Type.Examples examples)
      "Header" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        hdr <- Header.fromJson fromJson contentsJson
        Right (Type.Header hdr)
      "Table" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        tbl <- Table.fromJson fromJson contentsJson
        Right (Type.Table tbl)
      _ -> Left $ "unknown Doc tag: " <> Text.unpack tag
  _ -> Left "Doc must be an object"
  where
    parsePair :: Aeson.Value -> Either String (Int, Type.Doc)
    parsePair pairValue = case pairValue of
      Aeson.Array vec | Vector.length vec == 2 -> do
        n <- Helpers.fromJsonInt (vec Vector.! 0)
        doc <- fromJson (vec Vector.! 1)
        Right (n, doc)
      _ -> Left "OrderedList pair must be array of 2 elements"
    parseDefPair :: Aeson.Value -> Either String (Type.Doc, Type.Doc)
    parseDefPair defPairValue = case defPairValue of
      Aeson.Array vec | Vector.length vec == 2 -> do
        term <- fromJson (vec Vector.! 0)
        def <- fromJson (vec Vector.! 1)
        Right (term, def)
      _ -> Left "DefList pair must be array of 2 elements"

toJson :: Type.Doc -> Aeson.Value
toJson docValue = case docValue of
  Type.Empty -> Aeson.object ["tag" Aeson..= ("Empty" :: Text.Text)]
  Type.Append doc1 doc2 ->
    Aeson.object
      [ "tag" Aeson..= ("Append" :: Text.Text),
        "contents" Aeson..= [toJson doc1, toJson doc2]
      ]
  Type.String txt ->
    Aeson.object
      [ "tag" Aeson..= ("String" :: Text.Text),
        "contents" Aeson..= txt
      ]
  Type.Paragraph doc ->
    Aeson.object
      [ "tag" Aeson..= ("Paragraph" :: Text.Text),
        "contents" Aeson..= toJson doc
      ]
  Type.Identifier ident ->
    Aeson.object
      [ "tag" Aeson..= ("Identifier" :: Text.Text),
        "contents" Aeson..= Identifier.toJson ident
      ]
  Type.Module modLink ->
    Aeson.object
      [ "tag" Aeson..= ("Module" :: Text.Text),
        "contents" Aeson..= ModLink.toJson toJson modLink
      ]
  Type.Emphasis doc ->
    Aeson.object
      [ "tag" Aeson..= ("Emphasis" :: Text.Text),
        "contents" Aeson..= toJson doc
      ]
  Type.Monospaced doc ->
    Aeson.object
      [ "tag" Aeson..= ("Monospaced" :: Text.Text),
        "contents" Aeson..= toJson doc
      ]
  Type.Bold doc ->
    Aeson.object
      [ "tag" Aeson..= ("Bold" :: Text.Text),
        "contents" Aeson..= toJson doc
      ]
  Type.UnorderedList docs ->
    Aeson.object
      [ "tag" Aeson..= ("UnorderedList" :: Text.Text),
        "contents" Aeson..= fmap toJson docs
      ]
  Type.OrderedList pairs ->
    Aeson.object
      [ "tag" Aeson..= ("OrderedList" :: Text.Text),
        "contents" Aeson..= fmap (\(n, doc) -> [Aeson.toJSON n, toJson doc]) pairs
      ]
  Type.DefList pairs ->
    Aeson.object
      [ "tag" Aeson..= ("DefList" :: Text.Text),
        "contents" Aeson..= fmap (\(term, def) -> [toJson term, toJson def]) pairs
      ]
  Type.CodeBlock doc ->
    Aeson.object
      [ "tag" Aeson..= ("CodeBlock" :: Text.Text),
        "contents" Aeson..= toJson doc
      ]
  Type.Hyperlink hyperlink ->
    Aeson.object
      [ "tag" Aeson..= ("Hyperlink" :: Text.Text),
        "contents" Aeson..= Hyperlink.toJson toJson hyperlink
      ]
  Type.Pic pic ->
    Aeson.object
      [ "tag" Aeson..= ("Pic" :: Text.Text),
        "contents" Aeson..= Picture.toJson pic
      ]
  Type.MathInline txt ->
    Aeson.object
      [ "tag" Aeson..= ("MathInline" :: Text.Text),
        "contents" Aeson..= txt
      ]
  Type.MathDisplay txt ->
    Aeson.object
      [ "tag" Aeson..= ("MathDisplay" :: Text.Text),
        "contents" Aeson..= txt
      ]
  Type.AName txt ->
    Aeson.object
      [ "tag" Aeson..= ("AName" :: Text.Text),
        "contents" Aeson..= txt
      ]
  Type.Property txt ->
    Aeson.object
      [ "tag" Aeson..= ("Property" :: Text.Text),
        "contents" Aeson..= txt
      ]
  Type.Examples examples ->
    Aeson.object
      [ "tag" Aeson..= ("Examples" :: Text.Text),
        "contents" Aeson..= fmap Example.toJson examples
      ]
  Type.Header hdr ->
    Aeson.object
      [ "tag" Aeson..= ("Header" :: Text.Text),
        "contents" Aeson..= Header.toJson toJson hdr
      ]
  Type.Table tbl ->
    Aeson.object
      [ "tag" Aeson..= ("Table" :: Text.Text),
        "contents" Aeson..= Table.toJson toJson tbl
      ]
