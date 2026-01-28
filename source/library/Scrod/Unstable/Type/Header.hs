module Scrod.Unstable.Type.Header where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json
import qualified Scrod.Unstable.Type.Level as Level

-- | A section header with a level and title.
-- Mirrors 'Documentation.Haddock.Types.Header' from haddock-library.
data Header doc = MkHeader
  { level :: Level.Level,
    title :: doc
  }
  deriving (Eq, Ord, Show)

toJson :: (doc -> Json.Json) -> Header doc -> Json.Json
toJson f (MkHeader l t) =
  Json.object
    [ (Text.pack "level", Level.toJson l),
      (Text.pack "title", f t)
    ]
