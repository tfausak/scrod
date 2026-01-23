-- TODO: Flatten module hierarchy.
module Scrod.Unstable.Type.Table.Row where

import qualified Scrod.Unstable.Type.Table.Cell as Cell

-- | A table row containing cells.
-- Mirrors 'Documentation.Haddock.Types.TableRow' from haddock-library.
newtype Row a = MkRow
  { cells :: [Cell.Cell a]
  }
  deriving (Eq, Ord, Show)
