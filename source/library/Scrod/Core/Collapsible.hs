{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Collapsible where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.Header as Header
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | A collapsible section with a header and body content.
-- Corresponds to Haddock's collapsible headers, where the header title
-- is wrapped in bold syntax (@__title__@).
data Collapsible doc = MkCollapsible
  { header :: Header.Header doc,
    body :: [doc]
  }
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically (Collapsible doc)
