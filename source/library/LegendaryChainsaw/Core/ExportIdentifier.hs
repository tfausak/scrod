module LegendaryChainsaw.Core.ExportIdentifier where

import qualified LegendaryChainsaw.Core.Doc as Doc
import qualified LegendaryChainsaw.Core.ExportName as ExportName
import qualified LegendaryChainsaw.Core.Subordinates as Subordinates
import qualified LegendaryChainsaw.Core.Warning as Warning

-- | A named export: variable, type/class, or module re-export.
data ExportIdentifier = MkExportIdentifier
  { name :: ExportName.ExportName,
    subordinates :: Maybe Subordinates.Subordinates,
    warning :: Maybe Warning.Warning,
    doc :: Maybe Doc.Doc
  }
  deriving (Eq, Ord, Show)
