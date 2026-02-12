{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Category where

import qualified Data.Text as Text
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

newtype Category = MkCategory
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

deriving via Text.Text instance ToJson.ToJson Category

deriving via Text.Text instance Schema.ToSchema Category
