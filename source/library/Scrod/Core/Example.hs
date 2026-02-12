{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Example where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | An example expression with its expected result.
data Example = MkExample
  { expression :: Text.Text,
    result :: [Text.Text]
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically Example instance ToJson.ToJson Example

deriving via Generics.Generically Example instance Schema.ToSchema Example
