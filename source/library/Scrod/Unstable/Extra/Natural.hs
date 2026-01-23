module Scrod.Unstable.Extra.Natural where

import qualified Numeric.Natural as Natural

fromInt :: Int -> Maybe Natural.Natural
fromInt x =
  if x < 0
    then Nothing
    else Just $ fromIntegral x
