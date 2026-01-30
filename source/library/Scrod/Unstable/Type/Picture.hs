{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.Picture where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified GHC.Generics as Generics

-- | A picture/image reference.
-- Mirrors 'Documentation.Haddock.Types.Picture' from haddock-library,
-- but uses 'Text' instead of 'String'.
data Picture = MkPicture
  { uri :: Text.Text,
    title :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show, Generics.Generic)

instance Aeson.FromJSON Picture where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}

instance Aeson.ToJSON Picture where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}
