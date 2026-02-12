{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.ExportName where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Core.ExportNameKind as ExportNameKind

-- | A name in an export list, possibly annotated with @pattern@ or @type@.
data ExportName = MkExportName
  { kind :: Maybe ExportNameKind.ExportNameKind,
    name :: Text.Text
  }
  deriving (Eq, Generics.Generic, Ord, Show)
