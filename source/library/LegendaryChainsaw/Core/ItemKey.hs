module LegendaryChainsaw.Core.ItemKey where

import qualified Numeric.Natural as Natural

newtype ItemKey = MkItemKey
  { value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
