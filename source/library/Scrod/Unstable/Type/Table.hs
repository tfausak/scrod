module Scrod.Unstable.Type.Table where

import qualified Scrod.Unstable.Type.TableCell as TableCell

-- | A table with header and body rows.
-- Mirrors 'Documentation.Haddock.Types.Table' from haddock-library.
data Table doc = MkTable
  { headerRows :: [[TableCell.Cell doc]],
    bodyRows :: [[TableCell.Cell doc]]
  }
  deriving (Eq, Ord, Show)

empty :: Table a
empty =
  MkTable
    { headerRows = [],
      bodyRows = []
    }
