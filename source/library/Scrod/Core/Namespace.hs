{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Namespace where

import qualified GHC.Generics as Generics
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

-- | The namespace qualification for an identifier.
data Namespace
  = -- | @v'identifier'@ syntax
    Value
  | -- | @t'identifier'@ syntax
    Type
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically Namespace instance ToJson Namespace

deriving via Generics.Generically Namespace instance ToSchema Namespace
