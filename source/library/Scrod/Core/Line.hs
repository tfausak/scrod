{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Line where

import qualified Numeric.Natural as Natural
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

newtype Line = MkLine
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

deriving via Natural.Natural instance ToJson.ToJson Line

deriving via Natural.Natural instance Schema.ToSchema Line
