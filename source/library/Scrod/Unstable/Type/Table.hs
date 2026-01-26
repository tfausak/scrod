module Scrod.Unstable.Type.Table where

import qualified Scrod.Unstable.Type.TableCell as TableCell

-- | A table with header and body rows.
-- Mirrors 'Documentation.Haddock.Types.Table' from haddock-library.
data Table a = MkTable
  { headerRows :: [[TableCell.Cell a]],
    bodyRows :: [[TableCell.Cell a]]
  }
  deriving (Eq, Ord, Show)

empty :: Table a
empty =
  MkTable
    { headerRows = [],
      bodyRows = []
    }
