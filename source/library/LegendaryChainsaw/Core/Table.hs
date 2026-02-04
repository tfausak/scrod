module LegendaryChainsaw.Core.Table where

import qualified LegendaryChainsaw.Core.TableCell as TableCell

-- | A table with header and body rows.
data Table doc = MkTable
  { headerRows :: [[TableCell.Cell doc]],
    bodyRows :: [[TableCell.Cell doc]]
  }
  deriving (Eq, Ord, Show)
