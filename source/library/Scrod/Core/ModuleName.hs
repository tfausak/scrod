{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.ModuleName where

import qualified Data.Text as Text
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

newtype ModuleName = MkModuleName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

deriving via Text.Text instance ToJson ModuleName

deriving via Text.Text instance ToSchema ModuleName
