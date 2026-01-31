{-# LANGUAGE NamedFieldPuns #-}

module Scrod.Type.Located where

import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Scrod.Type.Location as Location

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
