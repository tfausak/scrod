module Scrod.Json.Line where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.Line as Type

fromJson :: Aeson.Value -> Either String Type.Line
fromJson value = fmap Type.MkLine (Helpers.fromJsonNatural value)

toJson :: Type.Line -> Aeson.Value
toJson (Type.MkLine n) = Aeson.toJSON n
