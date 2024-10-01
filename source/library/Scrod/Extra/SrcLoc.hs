module Scrod.Extra.SrcLoc where

import qualified GHC.Types.SrcLoc as SrcLoc

realSrcLocToShowS :: SrcLoc.RealSrcLoc -> ShowS
realSrcLocToShowS realSrcLoc =
  shows (SrcLoc.srcLocLine realSrcLoc)
    . showChar ':'
    . shows (SrcLoc.srcLocCol realSrcLoc)

realSrcSpanToShowS :: SrcLoc.RealSrcSpan -> ShowS
realSrcSpanToShowS realSrcSpan =
  realSrcLocToShowS (SrcLoc.realSrcSpanStart realSrcSpan)
    . showChar '-'
    . realSrcLocToShowS (SrcLoc.realSrcSpanEnd realSrcSpan)

srcLocToShowS :: SrcLoc.SrcLoc -> ShowS
srcLocToShowS srcLoc = case srcLoc of
  SrcLoc.RealSrcLoc realSrcLoc _ -> realSrcLocToShowS realSrcLoc
  SrcLoc.UnhelpfulLoc {} -> showString "{UnhelpfulLoc}"

srcSpanToShowS :: SrcLoc.SrcSpan -> ShowS
srcSpanToShowS srcSpan = case srcSpan of
  SrcLoc.RealSrcSpan realSrcSpan _ -> realSrcSpanToShowS realSrcSpan
  SrcLoc.UnhelpfulSpan {} -> showString "{UnhelpfulSpan}"
