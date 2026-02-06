module Scrod.Core.Section where

import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Header as Header

-- | A section heading in an export list.
newtype Section = MkSection
  { header :: Header.Header Doc.Doc
  }
  deriving (Eq, Ord, Show)
