{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Identifier where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Core.Namespace as Namespace
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

-- | An identifier reference in documentation.
data Identifier = MkIdentifier
  { namespace :: Maybe Namespace.Namespace,
    value :: Text.Text
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically Identifier instance ToJson Identifier

deriving via Generics.Generically Identifier instance ToSchema Identifier
