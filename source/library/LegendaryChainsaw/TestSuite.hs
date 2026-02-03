module LegendaryChainsaw.TestSuite where

import qualified LegendaryChainsaw.Executable.Config
import qualified LegendaryChainsaw.Executable.Flag
import qualified LegendaryChainsaw.Extra.Builder
import qualified LegendaryChainsaw.Extra.Either
import qualified LegendaryChainsaw.Extra.Maybe
import qualified LegendaryChainsaw.Extra.Parsec
import qualified LegendaryChainsaw.Extra.Read
import qualified LegendaryChainsaw.Json.Boolean
import qualified LegendaryChainsaw.Json.Null
import qualified LegendaryChainsaw.Spec as Spec
import qualified LegendaryChainsaw.Version

testSuite :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
testSuite s = do
  LegendaryChainsaw.Executable.Config.spec s
  LegendaryChainsaw.Executable.Flag.spec s
  LegendaryChainsaw.Extra.Builder.spec s
  LegendaryChainsaw.Extra.Either.spec s
  LegendaryChainsaw.Extra.Maybe.spec s
  LegendaryChainsaw.Extra.Parsec.spec s
  LegendaryChainsaw.Extra.Read.spec s
  LegendaryChainsaw.Json.Boolean.spec s
  LegendaryChainsaw.Json.Null.spec s
  LegendaryChainsaw.Version.spec s
