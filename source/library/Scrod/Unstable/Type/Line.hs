module Scrod.Unstable.Type.Line where

import qualified Numeric.Natural as Natural
import qualified Scrod.Unstable.Type.Json as Json

newtype Line = MkLine
  { value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

fromInt :: Int -> Maybe Line
fromInt x =
  if x < 1
    then Nothing
    else Just . MkLine $ fromIntegral x

toJson :: Line -> Json.Json
toJson (MkLine n) = Json.fromNatural n
