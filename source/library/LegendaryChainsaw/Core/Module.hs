module LegendaryChainsaw.Core.Module where

import qualified Data.Map as Map
import qualified LegendaryChainsaw.Core.Doc as Doc
import qualified LegendaryChainsaw.Core.Export as Export
import qualified LegendaryChainsaw.Core.Extension as Extension
import qualified LegendaryChainsaw.Core.Item as Item
import qualified LegendaryChainsaw.Core.Language as Language
import qualified LegendaryChainsaw.Core.Located as Located
import qualified LegendaryChainsaw.Core.ModuleName as ModuleName
import qualified LegendaryChainsaw.Core.Since as Since
import qualified LegendaryChainsaw.Core.Version as Version
import qualified LegendaryChainsaw.Core.Warning as Warning

data Module = MkModule
  { version :: Version.Version,
    language :: Maybe Language.Language,
    extensions :: Map.Map Extension.Extension Bool,
    documentation :: Doc.Doc,
    since :: Maybe Since.Since,
    name :: Maybe (Located.Located ModuleName.ModuleName),
    warning :: Maybe Warning.Warning,
    exports :: Maybe [Export.Export],
    items :: [Located.Located Item.Item]
  }
  deriving (Eq, Ord, Show)
