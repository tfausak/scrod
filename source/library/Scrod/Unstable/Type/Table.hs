module Scrod.Unstable.Type.Table where

import qualified Scrod.Unstable.Type.TableRow as TableRow

-- | A table with header and body rows.
-- Mirrors 'Documentation.Haddock.Types.Table' from haddock-library.
data Table a = MkTable
  { headerRows :: [TableRow.Row a],
    bodyRows :: [TableRow.Row a]
  }
  deriving (Eq, Ord, Show)
