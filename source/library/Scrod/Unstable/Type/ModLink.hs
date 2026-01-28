module Scrod.Unstable.Type.ModLink where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json
import qualified Scrod.Unstable.Type.ModuleName as ModuleName

-- | A link to a module with an optional label.
-- Mirrors 'Documentation.Haddock.Types.ModLink' from haddock-library,
-- but uses 'ModuleName' instead of 'String' for the module name.
data ModLink doc = MkModLink
  { name :: ModuleName.ModuleName,
    label :: Maybe doc
  }
  deriving (Eq, Ord, Show)

toJson :: (doc -> Json.Json) -> ModLink doc -> Json.Json
toJson f (MkModLink n l) =
  Json.object
    [ (Text.pack "name", ModuleName.toJson n),
      (Text.pack "label", maybe Json.Null f l)
    ]
