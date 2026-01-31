module Scrod.Type.Interface where

import qualified Data.Map as Map
import qualified Scrod.Type.Doc as Doc
import qualified Scrod.Type.Export as Export
import qualified Scrod.Type.Extension as Extension
import qualified Scrod.Type.Item as Item
import qualified Scrod.Type.Language as Language
import qualified Scrod.Type.Located as Located
import qualified Scrod.Type.ModuleName as ModuleName
import qualified Scrod.Type.Since as Since
import qualified Scrod.Type.Version as Version
import qualified Scrod.Type.Warning as Warning

data Interface = MkInterface
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
