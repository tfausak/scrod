module Scrod.Unstable.Type.Interface where

import qualified Data.Map as Map
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Language as Language
import qualified Scrod.Unstable.Type.Located as Located
import qualified Scrod.Unstable.Type.ModuleName as ModuleName

data Interface = MkInterface
  { language :: Maybe Language.Language,
    extensions :: Map.Map Extension.Extension Bool,
    moduleName :: Maybe (Located.Located ModuleName.ModuleName)
  }
  deriving (Eq, Ord, Show)
