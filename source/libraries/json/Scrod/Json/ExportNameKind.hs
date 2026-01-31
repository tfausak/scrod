{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.ExportNameKind where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.ExportNameKind as Type

fromJson :: Aeson.Value -> Either String Type.ExportNameKind
fromJson value = case value of
  Aeson.Object obj -> do
    tagJson <- Helpers.lookupField obj "tag"
    tag <- case tagJson of
      Aeson.String t -> Right t
      _ -> Left "tag must be a string"
    case tag of
      "Pattern" -> Right Type.Pattern
      "Type" -> Right Type.Type
      "Module" -> Right Type.Module
      _ -> Left $ "unknown ExportNameKind tag: " <> Text.unpack tag
  _ -> Left "ExportNameKind must be an object"

toJson :: Type.ExportNameKind -> Aeson.Value
toJson kind = Aeson.object ["tag" Aeson..= tag]
  where
    tag :: Text.Text
    tag = case kind of
      Type.Pattern -> "Pattern"
      Type.Type -> "Type"
      Type.Module -> "Module"
