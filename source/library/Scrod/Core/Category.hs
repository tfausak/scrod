{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Category where

import qualified Data.Text as Text
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

newtype Category = MkCategory
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

deriving via Text.Text instance ToJson Category

deriving via Text.Text instance ToSchema Category
