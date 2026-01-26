module Scrod.Unstable.Type.Section where

import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Level as Level

-- | A section heading in an export list.
-- Represents @-- * Section@, @-- ** Subsection@, etc.
data Section = MkSection
  { level :: Level.Level,
    title :: Doc.Doc
  }
  deriving (Eq, Ord, Show)
