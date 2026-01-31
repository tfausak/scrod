{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrod.Json.Column where

import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific
import qualified Scrod.Type.Column as Type

fromJson :: Aeson.Value -> Either String Type.Column
fromJson = \case
  Aeson.Number n -> case Scientific.floatingOrInteger n of
    Right i | i >= 0 -> Right (Type.MkColumn (fromInteger i))
    Right i -> Left $ "Column must be non-negative, got: " <> show i
    Left (_ :: Double) -> Left "Column must be an integer"
  _ -> Left "Column must be a number"

toJson :: Type.Column -> Aeson.Value
toJson (Type.MkColumn n) = Aeson.Number (Scientific.scientific (fromIntegral n) 0)
