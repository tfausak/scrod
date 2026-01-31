module Scrod.Json.Category where

import qualified Data.Aeson as Aeson
import qualified Scrod.Type.Category as Type

fromJson :: Aeson.Value -> Either String Type.Category
fromJson value = case value of
  Aeson.String txt -> Right (Type.MkCategory txt)
  _ -> Left "Category must be a string"

toJson :: Type.Category -> Aeson.Value
toJson (Type.MkCategory txt) = Aeson.String txt
