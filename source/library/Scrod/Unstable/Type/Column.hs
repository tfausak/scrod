{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrod.Unstable.Type.Column where

import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific
import qualified Numeric.Natural as Natural

newtype Column = MkColumn
  { value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

fromInt :: Int -> Maybe Column
fromInt x =
  if x < 1
    then Nothing
    else Just . MkColumn $ fromIntegral x

fromJson :: Aeson.Value -> Either String Column
fromJson = \case
  Aeson.Number n -> case Scientific.floatingOrInteger n of
    Right i | i >= 0 -> Right (MkColumn (fromInteger i))
    Right i -> Left $ "Column must be non-negative, got: " <> show i
    Left (_ :: Double) -> Left "Column must be an integer"
  _ -> Left "Column must be a number"

toJson :: Column -> Aeson.Value
toJson (MkColumn n) = Aeson.Number (Scientific.scientific (fromIntegral n) 0)
