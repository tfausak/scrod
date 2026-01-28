module Scrod.Unstable.Type.Section where

import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Header as Header
import qualified Scrod.Unstable.Type.Json as Json

-- | A section heading in an export list.
-- Represents @-- * Section@, @-- ** Subsection@, etc.
newtype Section = MkSection
  { header :: Header.Header Doc.Doc
  }
  deriving (Eq, Ord, Show)

toJson :: Section -> Json.Json
toJson (MkSection h) = Header.toJson Doc.toJson h
