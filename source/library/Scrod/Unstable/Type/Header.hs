module Scrod.Unstable.Type.Header where

import qualified Scrod.Unstable.Type.Level as Level

-- | A section header with a level and title.
-- Mirrors 'Documentation.Haddock.Types.Header' from haddock-library.
data Header doc = MkHeader
  { level :: Level.Level,
    title :: doc
  }
  deriving (Eq, Ord, Show)
