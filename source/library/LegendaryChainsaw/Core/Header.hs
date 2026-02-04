module LegendaryChainsaw.Core.Header where

import qualified LegendaryChainsaw.Core.Level as Level

-- | A section header with a level and title.
data Header doc = MkHeader
  { level :: Level.Level,
    title :: doc
  }
  deriving (Eq, Ord, Show)
