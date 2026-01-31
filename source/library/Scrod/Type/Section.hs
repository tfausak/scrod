module Scrod.Type.Section where

import qualified Scrod.Type.Doc as Doc
import qualified Scrod.Type.Header as Header

-- | A section heading in an export list.
-- Represents @-- * Section@, @-- ** Subsection@, etc.
newtype Section = MkSection
  { header :: Header.Header Doc.Doc
  }
  deriving (Eq, Ord, Show)
