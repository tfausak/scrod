{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Line where

import qualified Numeric.Natural as Natural
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

newtype Line = MkLine
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Natural.Natural
