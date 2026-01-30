{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.Namespace where

import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import qualified Scrod.Unstable.Type.JsonOptions as JsonOptions

-- | The namespace qualification for an identifier.
-- Mirrors 'Documentation.Haddock.Types.Namespace' from haddock-library,
-- but without the 'None' case (represented as 'Nothing' in 'Identifier').
data Namespace
  = -- | v'identifier' syntax
    Value
  | -- | t'identifier' syntax
    Type
  deriving (Eq, Ord, Show, Generics.Generic)

instance Aeson.FromJSON Namespace where
  parseJSON = Aeson.genericParseJSON JsonOptions.sumOptions

instance Aeson.ToJSON Namespace where
  toJSON = Aeson.genericToJSON JsonOptions.sumOptions
