module Scrod.Unstable.Extra.Maybe where

note :: a -> Maybe b -> Either a b
note x = maybe (Left x) Right
