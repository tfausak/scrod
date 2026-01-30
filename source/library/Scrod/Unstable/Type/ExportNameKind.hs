{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.ExportNameKind where

import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import qualified Scrod.Unstable.Type.JsonOptions as JsonOptions

-- | Namespace annotation for a name in an export list.
-- Corresponds to IEPattern and IEType from GHC's IEWrappedName.
-- Plain names (IEName) are represented as 'Nothing' in ExportName.
data ExportNameKind
  = -- | @pattern X@
    Pattern
  | -- | @type (:+:)@
    Type
  | -- | @module Data.List@
    Module
  deriving (Eq, Ord, Show, Generics.Generic)

instance Aeson.ToJSON ExportNameKind where
  toJSON = Aeson.genericToJSON JsonOptions.sumOptions
