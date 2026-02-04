module LegendaryChainsaw.Core.Location where

import qualified LegendaryChainsaw.Core.Column as Column
import qualified LegendaryChainsaw.Core.Line as Line

data Location = MkLocation
  { line :: Line.Line,
    column :: Column.Column
  }
  deriving (Eq, Ord, Show)
