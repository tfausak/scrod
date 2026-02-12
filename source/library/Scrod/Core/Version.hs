{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Version where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Numeric.Natural as Natural
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

newtype Version = MkVersion
  { unwrap :: NonEmpty.NonEmpty Natural.Natural
  }
  deriving (Eq, Ord, Show)

deriving via NonEmpty.NonEmpty Natural.Natural instance ToJson Version

deriving via NonEmpty.NonEmpty Natural.Natural instance ToSchema Version
