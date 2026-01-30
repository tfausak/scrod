module Scrod.Unstable.Type.Section where

import qualified Data.Aeson as Aeson
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Header as Header

-- | A section heading in an export list.
-- Represents @-- * Section@, @-- ** Subsection@, etc.
newtype Section = MkSection
  { header :: Header.Header Doc.Doc
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Section
fromJson v = fmap MkSection (Header.fromJson Doc.fromJson v)

toJson :: Section -> Aeson.Value
toJson (MkSection hdr) = Header.toJson Doc.toJson hdr
