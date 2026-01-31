{-# LANGUAGE LambdaCase #-}

module Scrod.Json.PackageName where

import qualified Data.Aeson as Aeson
import qualified Scrod.Type.PackageName as Type

fromJson :: Aeson.Value -> Either String Type.PackageName
fromJson = \case
  Aeson.String txt -> Right (Type.MkPackageName txt)
  _ -> Left "PackageName must be a string"

toJson :: Type.PackageName -> Aeson.Value
toJson (Type.MkPackageName txt) = Aeson.String txt
