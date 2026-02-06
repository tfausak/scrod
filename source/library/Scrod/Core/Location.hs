module Scrod.Core.Location where

import qualified Scrod.Core.Column as Column
import qualified Scrod.Core.Line as Line

data Location = MkLocation
  { line :: Line.Line,
    column :: Column.Column
  }
  deriving (Eq, Ord, Show)
