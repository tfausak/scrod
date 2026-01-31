{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.ExportName where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.ExportNameKind as ExportNameKind
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.ExportName as Type

fromJson :: Aeson.Value -> Either String Type.ExportName
fromJson = \case
  Aeson.Object obj -> do
    kindJson <- Helpers.lookupField obj "kind"
    k <- case kindJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (ExportNameKind.fromJson kindJson)
    nameJson <- Helpers.lookupField obj "name"
    n <- case nameJson of
      Aeson.String t -> Right t
      _ -> Left "name must be a string"
    Right $ Type.MkExportName {Type.kind = k, Type.name = n}
  _ -> Left "ExportName must be an object"

toJson :: Type.ExportName -> Aeson.Value
toJson (Type.MkExportName k n) =
  Aeson.object
    [ "kind" Aeson..= maybe Aeson.Null ExportNameKind.toJson k,
      "name" Aeson..= n
    ]
