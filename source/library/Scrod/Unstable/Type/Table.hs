{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Table where

import qualified Scrod.Unstable.Type.Json as Json
import qualified Scrod.Unstable.Type.TableCell as TableCell

-- | A table with header and body rows.
-- Mirrors 'Documentation.Haddock.Types.Table' from haddock-library.
data Table doc = MkTable
  { headerRows :: [[TableCell.Cell doc]],
    bodyRows :: [[TableCell.Cell doc]]
  }
  deriving (Eq, Ord, Show)

toJson :: (doc -> Json.Json) -> Table doc -> Json.Json
toJson f (MkTable h b) =
  Json.object
    [ ("headerRows", rowsToJson h),
      ("bodyRows", rowsToJson b)
    ]
  where
    rowsToJson = Json.fromList . fmap (Json.fromList . fmap (TableCell.toJson f))
