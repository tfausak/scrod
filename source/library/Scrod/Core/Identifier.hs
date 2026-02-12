{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Identifier where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Core.Namespace as Namespace
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | An identifier reference in documentation.
data Identifier = MkIdentifier
  { namespace :: Maybe Namespace.Namespace,
    value :: Text.Text
  }
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically Identifier
