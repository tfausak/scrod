module LegendaryChainsaw.Core.Located where

import qualified LegendaryChainsaw.Core.Location as Location

data Located a = MkLocated
  { location :: Location.Location,
    value :: a
  }
  deriving (Eq, Ord, Show)
