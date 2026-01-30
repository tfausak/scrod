{-# LANGUAGE DerivingVia #-}

module Scrod.Unstable.Type.Line where

import qualified Data.Aeson as Aeson
import qualified Numeric.Natural as Natural

newtype Line = MkLine
  { value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
  deriving (Aeson.ToJSON) via Natural.Natural

fromInt :: Int -> Maybe Line
fromInt x =
  if x < 1
    then Nothing
    else Just . MkLine $ fromIntegral x
