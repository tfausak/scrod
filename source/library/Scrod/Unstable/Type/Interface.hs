module Scrod.Unstable.Type.Interface where

import qualified Data.Map as Map
import qualified Data.Void as Void
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Language as Language
import qualified Scrod.Unstable.Type.Located as Located
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.Warning as Warning

data Interface = MkInterface
  { language :: Maybe Language.Language,
    extensions :: Map.Map Extension.Extension Bool,
    documentation :: Maybe (Haddock.DocH Void.Void Haddock.Identifier),
    name :: Maybe (Located.Located ModuleName.ModuleName),
    warning :: Maybe Warning.Warning
  }
  deriving (Eq, Show)
