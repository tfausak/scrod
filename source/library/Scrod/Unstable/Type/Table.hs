module Scrod.Unstable.Type.Table where

import qualified Scrod.Unstable.Type.Table.Row as Row

-- | A table with header and body rows.
-- Mirrors 'Documentation.Haddock.Types.Table' from haddock-library.
data Table a = MkTable
  { headerRows :: [Row.Row a],
    bodyRows :: [Row.Row a]
  }
  deriving (Eq, Ord, Show)
