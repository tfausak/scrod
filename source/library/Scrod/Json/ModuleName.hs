{-# LANGUAGE LambdaCase #-}

module Scrod.Json.ModuleName where

import qualified Data.Aeson as Aeson
import qualified Scrod.Type.ModuleName as Type

fromJson :: Aeson.Value -> Either String Type.ModuleName
fromJson = \case
  Aeson.String txt -> Right (Type.MkModuleName txt)
  _ -> Left "ModuleName must be a string"

toJson :: Type.ModuleName -> Aeson.Value
toJson (Type.MkModuleName txt) = Aeson.String txt
