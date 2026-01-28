module Scrod.Unstable.Type.Table where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json
import qualified Scrod.Unstable.Type.TableCell as TableCell

-- | A table with header and body rows.
-- Mirrors 'Documentation.Haddock.Types.Table' from haddock-library.
data Table doc = MkTable
  { headerRows :: [[TableCell.Cell doc]],
    bodyRows :: [[TableCell.Cell doc]]
  }
  deriving (Eq, Ord, Show)

empty :: Table a
empty =
  MkTable
    { headerRows = [],
      bodyRows = []
    }

toJson :: (doc -> Json.Json) -> Table doc -> Json.Json
toJson f (MkTable h b) =
  Json.object
    [ (Text.pack "headerRows", rowsToJson h),
      (Text.pack "bodyRows", rowsToJson b)
    ]
  where
    rowsToJson = Json.fromList . fmap (Json.fromList . fmap (TableCell.toJson f))
