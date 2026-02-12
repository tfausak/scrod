{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Section where

import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Header as Header
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

-- | A section heading in an export list.
newtype Section = MkSection
  { header :: Header.Header Doc.Doc
  }
  deriving (Eq, Ord, Show)

deriving via Header.Header Doc.Doc instance ToJson Section

deriving via Header.Header Doc.Doc instance ToSchema Section
