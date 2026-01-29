module Scrod.Unstable.Type.TableCell where

import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Scrod.Unstable.Type.Json as Json

-- | A table cell with colspan, rowspan, and contents.
-- Mirrors 'Documentation.Haddock.Types.TableCell' from haddock-library,
-- but uses 'Natural' instead of 'Int' for colspan and rowspan.
data Cell doc = MkCell
  { colspan :: Natural.Natural,
    rowspan :: Natural.Natural,
    contents :: doc
  }
  deriving (Eq, Ord, Show)

toJson :: (doc -> Json.Json) -> Cell doc -> Json.Json
toJson f (MkCell c r d) =
  Json.object
    [ (Text.pack "colspan", Json.fromNatural c),
      (Text.pack "rowspan", Json.fromNatural r),
      (Text.pack "contents", f d)
    ]
