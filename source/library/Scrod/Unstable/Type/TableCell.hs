{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.TableCell where

import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import qualified Numeric.Natural as Natural

-- | A table cell with colspan, rowspan, and contents.
-- Mirrors 'Documentation.Haddock.Types.TableCell' from haddock-library,
-- but uses 'Natural' instead of 'Int' for colspan and rowspan.
data Cell doc = MkCell
  { colspan :: Natural.Natural,
    rowspan :: Natural.Natural,
    contents :: doc
  }
  deriving (Eq, Ord, Show, Generics.Generic)

instance (Aeson.FromJSON doc) => Aeson.FromJSON (Cell doc) where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}

instance (Aeson.ToJSON doc) => Aeson.ToJSON (Cell doc) where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}
