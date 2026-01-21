{-# LANGUAGE NamedFieldPuns #-}

module Scrod.Unstable.Type.Location where

import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Scrod.Unstable.Type.Column as Column
import qualified Scrod.Unstable.Type.Line as Line

data Location = MkLocation
  { line :: Line.Line,
    column :: Column.Column
  }
  deriving (Eq, Ord, Show)

fromSrcLoc :: SrcLoc.SrcLoc -> Maybe Location
fromSrcLoc srcLoc = case srcLoc of
  SrcLoc.RealSrcLoc realSrcLoc _ -> do
    line <- Line.fromInt $ SrcLoc.srcLocLine realSrcLoc
    column <- Column.fromInt $ SrcLoc.srcLocCol realSrcLoc
    pure
      MkLocation
        { line,
          column
        }
  SrcLoc.UnhelpfulLoc _ -> Nothing

fromSrcSpan :: SrcLoc.SrcSpan -> Maybe Location
fromSrcSpan = fromSrcLoc . SrcLoc.srcSpanStart
