module Scrod.Core.Located where

import qualified Scrod.Core.Location as Location

data Located a = MkLocated
  { location :: Location.Location,
    value :: a
  }
  deriving (Eq, Ord, Show)
