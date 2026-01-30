{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.Header where

import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import qualified Scrod.Unstable.Type.Level as Level

-- | A section header with a level and title.
-- Mirrors 'Documentation.Haddock.Types.Header' from haddock-library.
data Header doc = MkHeader
  { level :: Level.Level,
    title :: doc
  }
  deriving (Eq, Ord, Show, Generics.Generic)

instance (Aeson.ToJSON doc) => Aeson.ToJSON (Header doc) where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}
