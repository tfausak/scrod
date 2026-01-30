{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Type.Warning where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Type.Category as Category
import qualified Scrod.Type.JsonHelpers as JsonHelpers

data Warning = MkWarning
  { category :: Category.Category,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Warning
fromJson = \case
  Aeson.Object obj -> do
    catJson <- JsonHelpers.lookupField obj "category"
    cat <- Category.fromJson catJson
    valJson <- JsonHelpers.lookupField obj "value"
    val <- case valJson of
      Aeson.String t -> Right t
      _ -> Left "value must be a string"
    Right $ MkWarning {category = cat, value = val}
  _ -> Left "Warning must be an object"

toJson :: Warning -> Aeson.Value
toJson (MkWarning cat val) =
  Aeson.object
    [ "category" Aeson..= Category.toJson cat,
      "value" Aeson..= val
    ]
