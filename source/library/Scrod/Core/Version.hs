{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Version where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Numeric.Natural as Natural
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

newtype Version = MkVersion
  { unwrap :: NonEmpty.NonEmpty Natural.Natural
  }
  deriving (Eq, Ord, Show)

deriving via NonEmpty.NonEmpty Natural.Natural instance ToJson.ToJson Version

deriving via NonEmpty.NonEmpty Natural.Natural instance Schema.ToSchema Version
