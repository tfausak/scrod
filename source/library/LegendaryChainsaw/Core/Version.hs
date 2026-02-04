module LegendaryChainsaw.Core.Version where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Numeric.Natural as Natural

newtype Version = MkVersion
  { value :: NonEmpty.NonEmpty Natural.Natural
  }
  deriving (Eq, Ord, Show)
