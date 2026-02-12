{-# LANGUAGE DerivingVia #-}

module Scrod.Core.ItemName where

import qualified Data.Text as Text
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

newtype ItemName = MkItemName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Text.Text
