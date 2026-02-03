module LegendaryChainsaw.TestSuite where

import qualified LegendaryChainsaw.Decimal
import qualified LegendaryChainsaw.Executable.Config
import qualified LegendaryChainsaw.Executable.Flag
import qualified LegendaryChainsaw.Extra.Builder
import qualified LegendaryChainsaw.Extra.Either
import qualified LegendaryChainsaw.Extra.Maybe
import qualified LegendaryChainsaw.Extra.Monoid
import qualified LegendaryChainsaw.Extra.Ord
import qualified LegendaryChainsaw.Extra.Parsec
import qualified LegendaryChainsaw.Extra.Read
import qualified LegendaryChainsaw.Extra.Semigroup
import qualified LegendaryChainsaw.Json.Array
import qualified LegendaryChainsaw.Json.Boolean
import qualified LegendaryChainsaw.Json.Null
import qualified LegendaryChainsaw.Json.Number
import qualified LegendaryChainsaw.Json.Object
import qualified LegendaryChainsaw.Json.Pair
import qualified LegendaryChainsaw.Json.String
import qualified LegendaryChainsaw.Spec as Spec
import qualified LegendaryChainsaw.Version

testSuite :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
testSuite s = do
  LegendaryChainsaw.Decimal.spec s
  LegendaryChainsaw.Executable.Config.spec s
  LegendaryChainsaw.Executable.Flag.spec s
  LegendaryChainsaw.Extra.Builder.spec s
  LegendaryChainsaw.Extra.Either.spec s
  LegendaryChainsaw.Extra.Maybe.spec s
  LegendaryChainsaw.Extra.Monoid.spec s
  LegendaryChainsaw.Extra.Ord.spec s
  LegendaryChainsaw.Extra.Parsec.spec s
  LegendaryChainsaw.Extra.Read.spec s
  LegendaryChainsaw.Extra.Semigroup.spec s
  LegendaryChainsaw.Json.Array.spec s
  LegendaryChainsaw.Json.Boolean.spec s
  LegendaryChainsaw.Json.Null.spec s
  LegendaryChainsaw.Json.Number.spec s
  LegendaryChainsaw.Json.Object.spec s
  LegendaryChainsaw.Json.Pair.spec s
  LegendaryChainsaw.Json.String.spec s
  LegendaryChainsaw.Version.spec s
