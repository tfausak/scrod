{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Extra.Ord where

import qualified LegendaryChainsaw.Spec as Spec

between :: (Ord a) => a -> a -> a -> Bool
between lo hi x = lo <= x && x <= hi

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'between $ do
    Spec.it s "fails when too low" $ do
      Spec.assertEq s (between 'b' 'd' 'a') False

    Spec.it s "succeeds when at the lower bound" $ do
      Spec.assertEq s (between 'b' 'd' 'b') True

    Spec.it s "succeeds when in the middle" $ do
      Spec.assertEq s (between 'b' 'd' 'c') True

    Spec.it s "succeeds when at the upper bound" $ do
      Spec.assertEq s (between 'b' 'd' 'd') True

    Spec.it s "fails when too high" $ do
      Spec.assertEq s (between 'b' 'd' 'e') False
