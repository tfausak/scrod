module Scrod.Type.Table where

import qualified Scrod.Type.TableCell as TableCell

-- | A table with header and body rows.
-- Mirrors 'Documentation.Haddock.Types.Table' from haddock-library.
data Table doc = MkTable
  { headerRows :: [[TableCell.Cell doc]],
    bodyRows :: [[TableCell.Cell doc]]
  }
  deriving (Eq, Ord, Show)
