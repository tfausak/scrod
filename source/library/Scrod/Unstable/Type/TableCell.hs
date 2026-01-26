module Scrod.Unstable.Type.TableCell where

import qualified Numeric.Natural as Natural

-- | A table cell with colspan, rowspan, and contents.
-- Mirrors 'Documentation.Haddock.Types.TableCell' from haddock-library,
-- but uses 'Natural' instead of 'Int' for colspan and rowspan.
data Cell doc = MkCell
  { colspan :: Natural.Natural,
    rowspan :: Natural.Natural,
    contents :: doc
  }
  deriving (Eq, Ord, Show)

empty :: doc -> Cell doc
empty x =
  MkCell
    { colspan = 1,
      rowspan = 1,
      contents = x
    }
