{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.PackageName where

import qualified Data.Text as Text
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

newtype PackageName = MkPackageName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

deriving via Text.Text instance ToJson PackageName

deriving via Text.Text instance ToSchema PackageName
