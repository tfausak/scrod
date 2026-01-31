{-# LANGUAGE LambdaCase #-}

module Scrod.Json.Extension where

import qualified Data.Aeson as Aeson
import qualified Scrod.Type.Extension as Type

fromJson :: Aeson.Value -> Either String Type.Extension
fromJson = \case
  Aeson.String txt -> Right (Type.MkExtension txt)
  _ -> Left "Extension must be a string"

toJson :: Type.Extension -> Aeson.Value
toJson (Type.MkExtension txt) = Aeson.String txt
