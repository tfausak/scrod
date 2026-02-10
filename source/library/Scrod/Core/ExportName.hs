module Scrod.Core.ExportName where

import qualified Data.Text as Text
import qualified Scrod.Core.ExportNameKind as ExportNameKind

-- | A name in an export list, possibly annotated with @pattern@ or @type@.
data ExportName = MkExportName
  { kind :: Maybe ExportNameKind.ExportNameKind,
    name :: Text.Text
  }
  deriving (Eq, Ord, Show)
