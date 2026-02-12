{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Line where

import qualified Numeric.Natural as Natural
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

newtype Line = MkLine
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

deriving via Natural.Natural instance ToJson Line

deriving via Natural.Natural instance ToSchema Line
