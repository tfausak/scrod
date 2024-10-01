module Scrod.Type.Position where

import qualified GHC.Hs
import qualified GHC.Types.SrcLoc as SrcLoc

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Eq, Ord, Show)

fromEpAnn :: GHC.Hs.EpAnn a -> Position
fromEpAnn = fromRealSrcSpan . GHC.Hs.epaLocationRealSrcSpan . GHC.Hs.entry

fromLocated :: SrcLoc.Located a -> Maybe Position
fromLocated = fromSrcSpan . SrcLoc.getLoc

fromLocatedAn :: GHC.Hs.LocatedAn a b -> Position
fromLocatedAn = fromEpAnn . SrcLoc.getLoc

fromRealSrcLoc :: SrcLoc.RealSrcLoc -> Position
fromRealSrcLoc x =
  Position
    { line = SrcLoc.srcLocLine x,
      column = SrcLoc.srcLocCol x
    }

fromRealSrcSpan :: SrcLoc.RealSrcSpan -> Position
fromRealSrcSpan = fromRealSrcLoc . SrcLoc.realSrcSpanStart

fromSrcSpan :: SrcLoc.SrcSpan -> Maybe Position
fromSrcSpan x = case x of
  SrcLoc.RealSrcSpan y _ -> Just $ fromRealSrcSpan y
  SrcLoc.UnhelpfulSpan {} -> Nothing
