module Scrod.Unstable.Type.Interface where

import qualified Data.Map as Map
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Language as Language
import qualified Scrod.Unstable.Type.Located as Located
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.Since as Since
import qualified Scrod.Unstable.Type.Warning as Warning

data Interface = MkInterface
  { language :: Maybe Language.Language,
    extensions :: Map.Map Extension.Extension Bool,
    documentation :: Doc.Doc,
    name :: Maybe (Located.Located ModuleName.ModuleName),
    since :: Maybe Since.Since,
    warning :: Maybe Warning.Warning
  }
  deriving (Eq, Ord, Show)
