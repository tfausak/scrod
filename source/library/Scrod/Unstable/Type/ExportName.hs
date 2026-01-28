module Scrod.Unstable.Type.ExportName where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.ExportNameKind as ExportNameKind
import qualified Scrod.Unstable.Type.Json as Json

-- | A name in an export list, possibly annotated with 'pattern' or 'type'.
-- Mirrors GHC's IEWrappedName but simplified.
data ExportName = MkExportName
  { kind :: Maybe ExportNameKind.ExportNameKind,
    name :: Text.Text
  }
  deriving (Eq, Ord, Show)

toJson :: ExportName -> Json.Json
toJson (MkExportName k n) =
  Json.object
    [ (Text.pack "kind", maybe Json.Null ExportNameKind.toJson k),
      (Text.pack "name", Json.fromText n)
    ]
