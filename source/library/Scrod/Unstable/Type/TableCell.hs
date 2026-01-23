module Scrod.Unstable.Type.TableCell where

import qualified Numeric.Natural as Natural

-- | A table cell with colspan, rowspan, and contents.
-- Mirrors 'Documentation.Haddock.Types.TableCell' from haddock-library,
-- but uses 'Natural' instead of 'Int' for colspan and rowspan.
data Cell a = MkCell
  { colspan :: Natural.Natural,
    rowspan :: Natural.Natural,
    contents :: a
  }
  deriving (Eq, Ord, Show)
