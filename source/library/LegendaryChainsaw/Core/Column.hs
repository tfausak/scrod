module LegendaryChainsaw.Core.Column where

import qualified Numeric.Natural as Natural

newtype Column = MkColumn
  { value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
