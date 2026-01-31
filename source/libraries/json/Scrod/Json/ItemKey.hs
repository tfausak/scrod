module Scrod.Json.ItemKey where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.ItemKey as Type

fromJson :: Aeson.Value -> Either String Type.ItemKey
fromJson value = fmap Type.MkItemKey (Helpers.fromJsonNatural value)

toJson :: Type.ItemKey -> Aeson.Value
toJson (Type.MkItemKey n) = Aeson.toJSON n
