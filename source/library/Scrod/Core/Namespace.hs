{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Namespace where

import qualified GHC.Generics as Generics
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | The namespace qualification for an identifier.
data Namespace
  = -- | @v'identifier'@ syntax
    Value
  | -- | @t'identifier'@ syntax
    Type
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically Namespace instance ToJson.ToJson Namespace

deriving via Generics.Generically Namespace instance Schema.ToSchema Namespace
