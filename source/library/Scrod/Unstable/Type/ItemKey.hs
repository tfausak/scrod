module Scrod.Unstable.Type.ItemKey where

import qualified Numeric.Natural as Natural
import qualified Scrod.Unstable.Type.Json as Json

newtype ItemKey = MkItemKey
  { value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

toJson :: ItemKey -> Json.Json
toJson (MkItemKey n) = Json.fromNatural n
