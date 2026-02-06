module Scrod.Core.Export where

import qualified Data.Text as Text
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.ExportIdentifier as ExportIdentifier
import qualified Scrod.Core.Section as Section

-- | A single entry in a module's export list.
data Export
  = -- | A named export: variable, type/class, or module re-export.
    Identifier ExportIdentifier.ExportIdentifier
  | -- | Section heading: @-- * Section@
    Group Section.Section
  | -- | Inline documentation: @-- | Some doc@
    Doc Doc.Doc
  | -- | Named doc reference: @-- $chunkName@
    DocNamed Text.Text
  deriving (Eq, Ord, Show)
