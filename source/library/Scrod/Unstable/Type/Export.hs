{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Export where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ExportIdentifier as ExportIdentifier
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers
import qualified Scrod.Unstable.Type.Section as Section

-- | A single entry in a module's export list.
-- Mirrors GHC's IE type but simplified.
data Export
  = -- | A named export: variable, type/class, or module re-export.
    Identifier ExportIdentifier.ExportIdentifier
  | -- | Section heading: @-- * Section@
    Group Section.Section
  | -- | Inline documentation: @-- | Some doc@
    Doc Doc.Doc
  | -- | Named doc reference: @-- $chunkName@
    DocNamed Text.Text
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Export
fromJson = \case
  Aeson.Object obj -> do
    tagJson <- JsonHelpers.lookupField obj "tag"
    tag <- case tagJson of
      Aeson.String t -> Right t
      _ -> Left "tag must be a string"
    case tag of
      "Identifier" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        ident <- ExportIdentifier.fromJson contentsJson
        Right (Identifier ident)
      "Group" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        section <- Section.fromJson contentsJson
        Right (Group section)
      "Doc" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        doc <- Doc.fromJson contentsJson
        Right (Doc doc)
      "DocNamed" -> do
        contentsJson <- JsonHelpers.lookupField obj "contents"
        txt <- case contentsJson of
          Aeson.String t -> Right t
          _ -> Left "DocNamed contents must be a string"
        Right (DocNamed txt)
      _ -> Left $ "unknown Export tag: " <> Text.unpack tag
  _ -> Left "Export must be an object"

toJson :: Export -> Aeson.Value
toJson = \case
  Identifier ident ->
    Aeson.object
      [ "tag" Aeson..= ("Identifier" :: Text.Text),
        "contents" Aeson..= ExportIdentifier.toJson ident
      ]
  Group section ->
    Aeson.object
      [ "tag" Aeson..= ("Group" :: Text.Text),
        "contents" Aeson..= Section.toJson section
      ]
  Doc doc ->
    Aeson.object
      [ "tag" Aeson..= ("Doc" :: Text.Text),
        "contents" Aeson..= Doc.toJson doc
      ]
  DocNamed txt ->
    Aeson.object
      [ "tag" Aeson..= ("DocNamed" :: Text.Text),
        "contents" Aeson..= txt
      ]
