module LegendaryChainsaw.Core.Export where

import qualified Data.Text as Text
import qualified LegendaryChainsaw.Core.Doc as Doc
import qualified LegendaryChainsaw.Core.ExportIdentifier as ExportIdentifier
import qualified LegendaryChainsaw.Core.Section as Section

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
