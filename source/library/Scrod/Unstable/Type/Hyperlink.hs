{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Hyperlink where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers

-- | A hyperlink with an optional label.
-- Mirrors 'Documentation.Haddock.Types.Hyperlink' from haddock-library,
-- but uses 'Text' instead of 'String' for the URL.
data Hyperlink doc = MkHyperlink
  { url :: Text.Text,
    label :: Maybe doc
  }
  deriving (Eq, Ord, Show)

fromJson :: (Aeson.Value -> Either String doc) -> Aeson.Value -> Either String (Hyperlink doc)
fromJson fromJsonDoc = \case
  Aeson.Object obj -> do
    urlJson <- JsonHelpers.lookupField obj "url"
    u <- case urlJson of
      Aeson.String t -> Right t
      _ -> Left "url must be a string"
    labelJson <- JsonHelpers.lookupField obj "label"
    lbl <- case labelJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (fromJsonDoc labelJson)
    Right $ MkHyperlink {url = u, label = lbl}
  _ -> Left "Hyperlink must be an object"

toJson :: (doc -> Aeson.Value) -> Hyperlink doc -> Aeson.Value
toJson toJsonDoc (MkHyperlink u lbl) =
  Aeson.object
    [ "url" Aeson..= u,
      "label" Aeson..= maybe Aeson.Null toJsonDoc lbl
    ]
