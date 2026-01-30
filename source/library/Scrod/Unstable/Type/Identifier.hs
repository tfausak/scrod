{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.Identifier where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Unstable.Type.Namespace as Namespace

-- | An identifier reference in documentation.
-- Combines an optional namespace (Value or Type) with the identifier text.
-- 'Nothing' namespace corresponds to plain 'identifier' syntax.
data Identifier = MkIdentifier
  { namespace :: Maybe Namespace.Namespace,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show, Generics.Generic)

instance Aeson.FromJSON Identifier where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}

instance Aeson.ToJSON Identifier where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}
