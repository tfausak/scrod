module Scrod.Json.Column where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.Column as Type

fromJson :: Aeson.Value -> Either String Type.Column
fromJson value = fmap Type.MkColumn (Helpers.fromJsonNatural value)

toJson :: Type.Column -> Aeson.Value
toJson (Type.MkColumn n) = Aeson.toJSON n
