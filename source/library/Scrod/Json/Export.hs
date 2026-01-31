{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Export where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Json.Doc as Doc
import qualified Scrod.Json.ExportIdentifier as ExportIdentifier
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.Section as Section
import qualified Scrod.Type.Export as Type

fromJson :: Aeson.Value -> Either String Type.Export
fromJson = \case
  Aeson.Object obj -> do
    tagJson <- Helpers.lookupField obj "tag"
    tag <- case tagJson of
      Aeson.String t -> Right t
      _ -> Left "tag must be a string"
    case tag of
      "Identifier" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        ident <- ExportIdentifier.fromJson contentsJson
        Right (Type.Identifier ident)
      "Group" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        section <- Section.fromJson contentsJson
        Right (Type.Group section)
      "Doc" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        doc <- Doc.fromJson contentsJson
        Right (Type.Doc doc)
      "DocNamed" -> do
        contentsJson <- Helpers.lookupField obj "contents"
        txt <- case contentsJson of
          Aeson.String t -> Right t
          _ -> Left "DocNamed contents must be a string"
        Right (Type.DocNamed txt)
      _ -> Left $ "unknown Export tag: " <> Text.unpack tag
  _ -> Left "Export must be an object"

toJson :: Type.Export -> Aeson.Value
toJson = \case
  Type.Identifier ident ->
    Aeson.object
      [ "tag" Aeson..= ("Identifier" :: Text.Text),
        "contents" Aeson..= ExportIdentifier.toJson ident
      ]
  Type.Group section ->
    Aeson.object
      [ "tag" Aeson..= ("Group" :: Text.Text),
        "contents" Aeson..= Section.toJson section
      ]
  Type.Doc doc ->
    Aeson.object
      [ "tag" Aeson..= ("Doc" :: Text.Text),
        "contents" Aeson..= Doc.toJson doc
      ]
  Type.DocNamed txt ->
    Aeson.object
      [ "tag" Aeson..= ("DocNamed" :: Text.Text),
        "contents" Aeson..= txt
      ]
