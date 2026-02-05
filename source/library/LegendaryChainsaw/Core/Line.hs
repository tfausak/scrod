module LegendaryChainsaw.Core.Line where

import qualified Numeric.Natural as Natural

newtype Line = MkLine
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
