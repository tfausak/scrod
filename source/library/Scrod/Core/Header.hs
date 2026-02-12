{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Header where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.Level as Level
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | A section header with a level and title.
data Header doc = MkHeader
  { level :: Level.Level,
    title :: doc
  }
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically (Header doc)
