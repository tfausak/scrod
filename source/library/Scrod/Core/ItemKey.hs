{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.ItemKey where

import qualified Numeric.Natural as Natural
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

newtype ItemKey = MkItemKey
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

deriving via Natural.Natural instance ToJson ItemKey

deriving via Natural.Natural instance ToSchema ItemKey
