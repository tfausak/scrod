{-# LANGUAGE DerivingVia #-}

module Scrod.Unstable.Type.Column where

import qualified Data.Aeson as Aeson
import qualified Numeric.Natural as Natural

newtype Column = MkColumn
  { value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via Natural.Natural

fromInt :: Int -> Maybe Column
fromInt x =
  if x < 1
    then Nothing
    else Just . MkColumn $ fromIntegral x
