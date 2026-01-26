module Scrod.Unstable.Type.SubordinateIdentifier where

import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.ExportName as ExportName
import qualified Scrod.Unstable.Type.Warning as Warning

-- | A subordinate export item: constructor, field, or pattern.
-- Similar to 'ExportIdentifier' but without subordinates of its own.
-- This separates constructors into their own export items.
data SubordinateIdentifier = MkSubordinateIdentifier
  { name :: ExportName.ExportName,
    warning :: Maybe Warning.Warning,
    doc :: Maybe Doc.Doc
  }
  deriving (Eq, Ord, Show)
