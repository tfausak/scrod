{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrod.Json.ItemKey where

import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific
import qualified Scrod.Type.ItemKey as Type

fromJson :: Aeson.Value -> Either String Type.ItemKey
fromJson = \case
  Aeson.Number n -> case Scientific.floatingOrInteger n of
    Right i | i >= 0 -> Right (Type.MkItemKey (fromInteger i))
    Right i -> Left $ "ItemKey must be non-negative, got: " <> show i
    Left (_ :: Double) -> Left "ItemKey must be an integer"
  _ -> Left "ItemKey must be a number"

toJson :: Type.ItemKey -> Aeson.Value
toJson (Type.MkItemKey n) = Aeson.Number (Scientific.scientific (fromIntegral n) 0)
