module Scrod.Core.Module where

import qualified Data.Map as Map
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Export as Export
import qualified Scrod.Core.Extension as Extension
import qualified Scrod.Core.Import as Import
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.Language as Language
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.ModuleName as ModuleName
import qualified Scrod.Core.Since as Since
import qualified Scrod.Core.Version as Version
import qualified Scrod.Core.Warning as Warning

data Module = MkModule
  { version :: Version.Version,
    language :: Maybe Language.Language,
    extensions :: Map.Map Extension.Extension Bool,
    documentation :: Doc.Doc,
    since :: Maybe Since.Since,
    name :: Maybe (Located.Located ModuleName.ModuleName),
    warning :: Maybe Warning.Warning,
    exports :: Maybe [Export.Export],
    imports :: [Import.Import],
    items :: [Located.Located Item.Item]
  }
  deriving (Eq, Ord, Show)
