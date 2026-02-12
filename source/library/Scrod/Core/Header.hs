{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Header where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.Level as Level
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

-- | A section header with a level and title.
data Header doc = MkHeader
  { level :: Level.Level,
    title :: doc
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically (Header doc) instance (ToJson doc) => ToJson (Header doc)

deriving via Generics.Generically (Header doc) instance (ToSchema doc) => ToSchema (Header doc)
