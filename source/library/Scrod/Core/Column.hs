module Scrod.Core.Column where

import qualified Numeric.Natural as Natural

newtype Column = MkColumn
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
