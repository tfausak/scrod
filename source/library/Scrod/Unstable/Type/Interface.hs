module Scrod.Unstable.Type.Interface where

import qualified Data.Map as Map
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Export as Export
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Language as Language
import qualified Scrod.Unstable.Type.Located as Located
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.Since as Since
import qualified Scrod.Unstable.Type.Warning as Warning
import qualified Scrod.Unstable.Type.Item as Item

data Interface = MkInterface
  { language :: Maybe Language.Language,
    extensions :: Map.Map Extension.Extension Bool,
    documentation :: Doc.Doc,
    since :: Maybe Since.Since,
    name :: Maybe (Located.Located ModuleName.ModuleName),
    warning :: Maybe Warning.Warning,
    exports :: Maybe [Export.Export],
    items :: [Located.Located Item.Item]
  }
  deriving (Eq, Ord, Show)
