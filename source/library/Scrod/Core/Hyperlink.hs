{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Hyperlink where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | A hyperlink with an optional label.
data Hyperlink doc = MkHyperlink
  { url :: Text.Text,
    label :: Maybe doc
  }
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically (Hyperlink doc)
