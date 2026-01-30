{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.ModLink where

import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import qualified Scrod.Unstable.Type.ModuleName as ModuleName

-- | A link to a module with an optional label.
-- Mirrors 'Documentation.Haddock.Types.ModLink' from haddock-library,
-- but uses 'ModuleName' instead of 'String' for the module name.
data ModLink doc = MkModLink
  { name :: ModuleName.ModuleName,
    label :: Maybe doc
  }
  deriving (Eq, Ord, Show, Generics.Generic)

instance (Aeson.ToJSON doc) => Aeson.ToJSON (ModLink doc) where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}
