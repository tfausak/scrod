module Scrod.Unstable.Type.TableRow where

import qualified Scrod.Unstable.Type.TableCell as TableCell

-- | A table row containing cells.
-- Mirrors 'Documentation.Haddock.Types.TableRow' from haddock-library.
newtype Row a = MkRow
  { cells :: [TableCell.Cell a]
  }
  deriving (Eq, Ord, Show)
