module Scrod.Type.ExportName where

import qualified Data.Text as Text
import qualified Scrod.Type.ExportNameKind as ExportNameKind

-- | A name in an export list, possibly annotated with 'pattern' or 'type'.
-- Mirrors GHC's IEWrappedName but simplified.
data ExportName = MkExportName
  { kind :: Maybe ExportNameKind.ExportNameKind,
    name :: Text.Text
  }
  deriving (Eq, Ord, Show)
