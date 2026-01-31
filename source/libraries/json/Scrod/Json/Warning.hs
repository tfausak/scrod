{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Warning where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Category as Category
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.Warning as Type

fromJson :: Aeson.Value -> Either String Type.Warning
fromJson = \case
  Aeson.Object obj -> do
    catJson <- Helpers.lookupField obj "category"
    cat <- Category.fromJson catJson
    valJson <- Helpers.lookupField obj "value"
    val <- case valJson of
      Aeson.String t -> Right t
      _ -> Left "value must be a string"
    Right $ Type.MkWarning {Type.category = cat, Type.value = val}
  _ -> Left "Warning must be an object"

toJson :: Type.Warning -> Aeson.Value
toJson (Type.MkWarning cat val) =
  Aeson.object
    [ "category" Aeson..= Category.toJson cat,
      "value" Aeson..= val
    ]
