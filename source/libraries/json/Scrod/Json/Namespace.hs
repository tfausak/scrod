{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Namespace where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.Namespace as Type

fromJson :: Aeson.Value -> Either String Type.Namespace
fromJson = \case
  Aeson.Object obj -> do
    tagJson <- Helpers.lookupField obj "tag"
    tag <- case tagJson of
      Aeson.String t -> Right t
      _ -> Left "tag must be a string"
    case tag of
      "Value" -> Right Type.Value
      "Type" -> Right Type.Type
      _ -> Left $ "unknown Namespace tag: " <> Text.unpack tag
  _ -> Left "Namespace must be an object"

toJson :: Type.Namespace -> Aeson.Value
toJson ns = Aeson.object ["tag" Aeson..= tag]
  where
    tag :: Text.Text
    tag = case ns of
      Type.Value -> "Value"
      Type.Type -> "Type"
