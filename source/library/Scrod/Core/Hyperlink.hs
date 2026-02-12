{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Hyperlink where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

-- | A hyperlink with an optional label.
data Hyperlink doc = MkHyperlink
  { url :: Text.Text,
    label :: Maybe doc
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically (Hyperlink doc) instance (ToJson doc) => ToJson (Hyperlink doc)

deriving via Generics.Generically (Hyperlink doc) instance (ToSchema doc) => ToSchema (Hyperlink doc)
