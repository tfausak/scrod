module Scrod.Json.ItemName where

import qualified Data.Aeson as Aeson
import qualified Scrod.Type.ItemName as Type

fromJson :: Aeson.Value -> Either String Type.ItemName
fromJson value = case value of
  Aeson.String txt -> Right (Type.MkItemName txt)
  _ -> Left "ItemName must be a string"

toJson :: Type.ItemName -> Aeson.Value
toJson (Type.MkItemName txt) = Aeson.String txt
