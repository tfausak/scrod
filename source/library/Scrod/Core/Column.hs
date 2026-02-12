{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Column where

import qualified Numeric.Natural as Natural
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

newtype Column = MkColumn
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

deriving via Natural.Natural instance ToJson.ToJson Column

deriving via Natural.Natural instance Schema.ToSchema Column
