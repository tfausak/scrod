{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.PackageName where

import qualified Data.Text as Text
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

newtype PackageName = MkPackageName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

deriving via Text.Text instance ToJson.ToJson PackageName

deriving via Text.Text instance Schema.ToSchema PackageName
