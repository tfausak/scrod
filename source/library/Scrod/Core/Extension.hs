{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Extension where

import qualified Data.Text as Text
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

newtype Extension = MkExtension
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

deriving via Text.Text instance ToJson.ToJson Extension

deriving via Text.Text instance Schema.ToSchema Extension
