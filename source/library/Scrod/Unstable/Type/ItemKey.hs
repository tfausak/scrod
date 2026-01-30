{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrod.Unstable.Type.ItemKey where

import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific
import qualified Numeric.Natural as Natural

newtype ItemKey = MkItemKey
  { value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String ItemKey
fromJson = \case
  Aeson.Number n -> case Scientific.floatingOrInteger n of
    Right i | i >= 0 -> Right (MkItemKey (fromInteger i))
    Right i -> Left $ "ItemKey must be non-negative, got: " <> show i
    Left (_ :: Double) -> Left "ItemKey must be an integer"
  _ -> Left "ItemKey must be a number"

toJson :: ItemKey -> Aeson.Value
toJson (MkItemKey n) = Aeson.Number (Scientific.scientific (fromIntegral n) 0)
