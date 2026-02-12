{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Section where

import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Header as Header
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | A section heading in an export list.
newtype Section = MkSection
  { header :: Header.Header Doc.Doc
  }
  deriving (Eq, Ord, Show)

deriving via Header.Header Doc.Doc instance ToJson.ToJson Section

deriving via Header.Header Doc.Doc instance Schema.ToSchema Section
