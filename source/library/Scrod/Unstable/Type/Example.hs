{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.Example where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified GHC.Generics as Generics

-- | An example expression with its expected result.
-- Mirrors 'Documentation.Haddock.Types.Example' from haddock-library,
-- but uses 'Text' instead of 'String'.
data Example = MkExample
  { expression :: Text.Text,
    result :: [Text.Text]
  }
  deriving (Eq, Ord, Show, Generics.Generic)

instance Aeson.ToJSON Example where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}
