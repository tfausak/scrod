module Scrod.Unstable.Type.Column where

import qualified Numeric.Natural as Natural
import qualified Scrod.Unstable.Type.Json as Json

newtype Column = MkColumn
  { value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

fromInt :: Int -> Maybe Column
fromInt x =
  if x < 1
    then Nothing
    else Just . MkColumn $ fromIntegral x

toJson :: Column -> Json.Json
toJson (MkColumn n) = Json.fromNatural n
