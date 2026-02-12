{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.ItemKey where

import qualified Numeric.Natural as Natural
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

newtype ItemKey = MkItemKey
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

deriving via Natural.Natural instance ToJson.ToJson ItemKey

deriving via Natural.Natural instance Schema.ToSchema ItemKey
