{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrod.Unstable.Type.Line where

import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific
import qualified Numeric.Natural as Natural

newtype Line = MkLine
  { value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

fromInt :: Int -> Maybe Line
fromInt x =
  if x < 1
    then Nothing
    else Just . MkLine $ fromIntegral x

fromJson :: Aeson.Value -> Either String Line
fromJson = \case
  Aeson.Number n -> case Scientific.floatingOrInteger n of
    Right i | i >= 0 -> Right (MkLine (fromInteger i))
    Right i -> Left $ "Line must be non-negative, got: " <> show i
    Left (_ :: Double) -> Left "Line must be an integer"
  _ -> Left "Line must be a number"

toJson :: Line -> Aeson.Value
toJson (MkLine n) = Aeson.Number (Scientific.scientific (fromIntegral n) 0)
