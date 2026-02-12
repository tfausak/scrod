{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Language where

import qualified Data.Text as Text
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

newtype Language = MkLanguage
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

deriving via Text.Text instance ToJson Language

deriving via Text.Text instance ToSchema Language
