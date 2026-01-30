{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.Item where

import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ItemKey as ItemKey
import qualified Scrod.Unstable.Type.ItemName as ItemName

data Item = MkItem
  { key :: ItemKey.ItemKey,
    parentKey :: Maybe ItemKey.ItemKey,
    name :: Maybe ItemName.ItemName,
    documentation :: Doc.Doc
  }
  deriving (Eq, Ord, Show, Generics.Generic)

instance Aeson.FromJSON Item where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}

instance Aeson.ToJSON Item where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}
