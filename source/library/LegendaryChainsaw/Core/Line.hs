module LegendaryChainsaw.Core.Line where

import qualified Numeric.Natural as Natural

newtype Line = MkLine
  { value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
