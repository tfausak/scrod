{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Extension where

import qualified Data.Text as Text
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

newtype Extension = MkExtension
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

deriving via Text.Text instance ToJson Extension

deriving via Text.Text instance ToSchema Extension
