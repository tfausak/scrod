module Scrod.Type.Column where

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
