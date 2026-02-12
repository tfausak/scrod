{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.ModuleName where

import qualified Data.Text as Text
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

newtype ModuleName = MkModuleName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

deriving via Text.Text instance ToJson.ToJson ModuleName

deriving via Text.Text instance Schema.ToSchema ModuleName
