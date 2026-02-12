{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

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

deriving via Generics.Generically (Header doc) instance (ToJson.ToJson doc) => ToJson.ToJson (Header doc)

deriving via Generics.Generically (Header doc) instance (Schema.ToSchema doc) => Schema.ToSchema (Header doc)
