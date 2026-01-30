{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.Hyperlink where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified GHC.Generics as Generics

-- | A hyperlink with an optional label.
-- Mirrors 'Documentation.Haddock.Types.Hyperlink' from haddock-library,
-- but uses 'Text' instead of 'String' for the URL.
data Hyperlink doc = MkHyperlink
  { url :: Text.Text,
    label :: Maybe doc
  }
  deriving (Eq, Ord, Show, Generics.Generic)

instance (Aeson.ToJSON doc) => Aeson.ToJSON (Hyperlink doc) where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}
