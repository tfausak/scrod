{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Language where

import qualified Data.Text as Text
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

newtype Language = MkLanguage
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

deriving via Text.Text instance ToJson.ToJson Language

deriving via Text.Text instance Schema.ToSchema Language
