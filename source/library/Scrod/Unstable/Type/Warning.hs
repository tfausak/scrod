{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.Warning where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Unstable.Type.Category as Category

data Warning = MkWarning
  { category :: Category.Category,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show, Generics.Generic)

instance Aeson.ToJSON Warning where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}
