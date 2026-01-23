module Scrod.Unstable.Type.Header where

import qualified Data.Word as Word

-- | A section header with a level and title.
-- Mirrors 'Documentation.Haddock.Types.Header' from haddock-library,
-- but uses 'Word8' instead of 'Int' for the level (1-6 inclusive).
data Header a = MkHeader
  { level :: Word.Word8, -- TODO: Create custom type to support only levels 1 through 6.
    title :: a
  }
  deriving (Eq, Ord, Show)
