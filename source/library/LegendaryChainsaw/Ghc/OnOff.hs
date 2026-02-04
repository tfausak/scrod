{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Ghc.OnOff where

import qualified GHC.Driver.DynFlags as DynFlags
import qualified LegendaryChainsaw.Spec as Spec

onOff :: (a -> b) -> (a -> b) -> DynFlags.OnOff a -> b
onOff f g x = case x of
  DynFlags.On y -> f y
  DynFlags.Off y -> g y

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'onOff $ do
    Spec.it s "works with On" $ do
      Spec.assertEq s (onOff succ pred $ DynFlags.On 'b') 'c'

    Spec.it s "works with Off" $ do
      Spec.assertEq s (onOff succ pred $ DynFlags.Off 'b') 'a'
