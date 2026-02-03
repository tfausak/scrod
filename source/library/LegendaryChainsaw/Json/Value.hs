{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Json.Value where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Json.Array as Array
import qualified LegendaryChainsaw.Json.Boolean as Boolean
import qualified LegendaryChainsaw.Json.Null as Null
import qualified LegendaryChainsaw.Json.Number as Number
import qualified LegendaryChainsaw.Json.Object as Object
import qualified LegendaryChainsaw.Json.Pair as Pair
import qualified LegendaryChainsaw.Json.String as String
import qualified LegendaryChainsaw.Decimal as Decimal
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

data Value
  = Null Null.Null
  | Boolean Boolean.Boolean
  | Number Number.Number
  | String String.String
  | Array (Array.Array Value)
  | Object (Object.Object Value)
  deriving (Eq, Ord, Show)

decode :: Parsec.Stream s m Char => Parsec.ParsecT s u m Value
decode = Parsec.between (Parsec.many Parsec.blank) (Parsec.many Parsec.blank) $
  Parsec.choice
    [ Null <$> Null.decode
    , Boolean <$> Boolean.decode
    , Number <$> Number.decode
    , String <$> String.decode
    , Array <$> Array.decode decode
    , Object <$> Object.decode decode
    ]

encode :: Value -> Builder.Builder
encode v = case v of
  Null n -> Null.encode n
  Boolean b -> Boolean.encode b
  Number n -> Number.encode n
  String s -> String.encode s
  Array a -> Array.encode encode a
  Object o -> Object.encode encode o

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  let null_ = Null Null.MkNull
      boolean = Boolean . Boolean.MkBoolean
      number m = Number . Number.MkNumber . Decimal.mkDecimal m
      string = String . String.MkString . Text.pack
      array = Array . Array.MkArray
      object = Object . Object.MkObject . fmap (\ (n, v) -> Pair.MkPair (String.MkString $ Text.pack n) v)

  Spec.named s 'decode $ do
    Spec.it s "parses null" $ do
      Spec.assertEq s (Parsec.parseString decode "null") $ Just null_

    Spec.it s "parses true" $ do
      Spec.assertEq s (Parsec.parseString decode "true") . Just $ boolean True

    Spec.it s "parses false" $ do
      Spec.assertEq s (Parsec.parseString decode "false") . Just $ boolean False

    Spec.it s "parses number" $ do
      Spec.assertEq s (Parsec.parseString decode "123") . Just $ number 123 0

    Spec.it s "parses string" $ do
      Spec.assertEq s (Parsec.parseString decode "\"hello\"") . Just $ string "hello"

    Spec.it s "parses empty array" $ do
      Spec.assertEq s (Parsec.parseString decode "[]") . Just $ array []

    Spec.it s "parses array with values" $ do
      Spec.assertEq s (Parsec.parseString decode "[1, \"a\", true]") . Just $ array [number 1 0, string "a", Boolean (Boolean.MkBoolean True)]

    Spec.it s "parses empty object" $ do
      Spec.assertEq s (Parsec.parseString decode "{}") . Just $ object []

    Spec.it s "parses object with values" $ do
      Spec.assertEq s (Parsec.parseString decode "{\"a\": 1, \"b\": \"x\"}") . Just $ object [("a", number 1 0), ("b", string "x")]

    Spec.it s "parses nested structure" $ do
      Spec.assertEq s (Parsec.parseString decode "{\"items\": [1, 2], \"name\": \"test\"}") . Just $ object [("items", array [number 1 0, number 2 0]), ("name", string "test")]

    Spec.it s "parses with leading whitespace" $ do
      Spec.assertEq s (Parsec.parseString decode "  null") $ Just null_

    Spec.it s "parses with trailing whitespace" $ do
      Spec.assertEq s (Parsec.parseString decode "null  ") $ Just null_

    Spec.it s "fails with invalid input" $ do
      Spec.assertEq s (Parsec.parseString decode "invalid") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes null" $ do
      Spec.assertEq s (Builder.toString $ encode null_) "null"

    Spec.it s "encodes true" $ do
      Spec.assertEq s (Builder.toString . encode $ boolean True) "true"

    Spec.it s "encodes false" $ do
      Spec.assertEq s (Builder.toString . encode $ boolean False) "false"

    Spec.it s "encodes number" $ do
      Spec.assertEq s (Builder.toString . encode $ number 123 0) "123e0"

    Spec.it s "encodes string" $ do
      Spec.assertEq s (Builder.toString . encode $ string "hello") "\"hello\""

    Spec.it s "encodes empty array" $ do
      Spec.assertEq s (Builder.toString . encode $ array []) "[]"

    Spec.it s "encodes array with values" $ do
      Spec.assertEq s (Builder.toString . encode $ array [number 1 0, string "a"]) "[1e0,\"a\"]"

    Spec.it s "encodes empty object" $ do
      Spec.assertEq s (Builder.toString . encode $ object []) "{}"

    Spec.it s "encodes object with values" $ do
      Spec.assertEq s (Builder.toString . encode $ object [("a", number 1 0)]) "{\"a\":1e0}"

    Spec.it s "encodes nested structure" $ do
      Spec.assertEq s (Builder.toString . encode $ object [("items", array [number 1 0])]) "{\"items\":[1e0]}"

