module LegendaryChainsaw.Core.TableCell where

import qualified Numeric.Natural as Natural

-- | A table cell with colspan, rowspan, and contents.
data Cell doc = MkCell
  { colspan :: Natural.Natural,
    rowspan :: Natural.Natural,
    contents :: doc
  }
  deriving (Eq, Ord, Show)
