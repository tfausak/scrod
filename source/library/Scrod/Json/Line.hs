{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrod.Json.Line where

import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific
import qualified Scrod.Type.Line as Type

fromJson :: Aeson.Value -> Either String Type.Line
fromJson = \case
  Aeson.Number n -> case Scientific.floatingOrInteger n of
    Right i | i >= 0 -> Right (Type.MkLine (fromInteger i))
    Right i -> Left $ "Line must be non-negative, got: " <> show i
    Left (_ :: Double) -> Left "Line must be an integer"
  _ -> Left "Line must be a number"

toJson :: Type.Line -> Aeson.Value
toJson (Type.MkLine n) = Aeson.Number (Scientific.scientific (fromIntegral n) 0)
