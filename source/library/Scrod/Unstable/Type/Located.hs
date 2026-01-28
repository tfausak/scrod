{-# LANGUAGE NamedFieldPuns #-}

module Scrod.Unstable.Type.Located where

import qualified Data.Text as Text
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Scrod.Unstable.Type.Json as Json
import qualified Scrod.Unstable.Type.Location as Location

data Located a = MkLocated
  { location :: Location.Location,
    value :: a
  }
  deriving (Eq, Ord, Show)

fromGhc :: SrcLoc.Located a -> Maybe (Located a)
fromGhc located = do
  location <- Location.fromSrcSpan $ SrcLoc.getLoc located
  pure
    MkLocated
      { location,
        value = SrcLoc.unLoc located
      }

toJson :: (a -> Json.Json) -> Located a -> Json.Json
toJson f (MkLocated loc v) =
  Json.object
    [ (Text.pack "location", Location.toJson loc),
      (Text.pack "value", f v)
    ]
