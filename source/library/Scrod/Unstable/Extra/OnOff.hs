module Scrod.Unstable.Extra.OnOff where

import qualified GHC.Driver.DynFlags as DynFlags

onOff :: (a -> b) -> (a -> b) -> DynFlags.OnOff a -> b
onOff f g x = case x of
  DynFlags.On y -> f y
  DynFlags.Off y -> g y
