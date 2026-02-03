module LegendaryChainsaw.TestSuite where

import qualified LegendaryChainsaw.Spec as Spec
import qualified LegendaryChainsaw.Version

testSuite :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
testSuite s = do
  LegendaryChainsaw.Version.spec s
