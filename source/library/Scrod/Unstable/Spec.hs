{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 #-}

module Scrod.Unstable.Spec
  ( spec,
    jsonSpec,
  )
where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Heck (Test, assertEq, describe, it)
import Scrod.Unstable.Extra.Heck (assertSatisfies, expectRight)
import qualified Scrod.Unstable.Type.Decimal as Decimal
import qualified Scrod.Unstable.Type.Json as Json

-- | All extract tests have been moved to JSON files in test-data/
spec :: (Monad n) => Test m n -> n ()
spec t = describe t "extract" $ do
  -- invalid tests moved to test-data/invalid/
  -- unsupported tests moved to test-data/unsupported/
  -- language tests moved to test-data/language/
  -- extensions tests moved to test-data/extensions/
  -- documentation tests moved to test-data/documentation/
  -- since tests moved to test-data/since/
  -- name tests moved to test-data/name/
  -- warning tests moved to test-data/warning/
  -- exports tests moved to test-data/exports/
  -- items tests moved to test-data/items/
  pure ()

jsonSpec :: (MonadFail m, Monad n) => Test m n -> n ()
jsonSpec t = describe t "json" $ do
  describe t "parse" $ do
    it t "parses null" $ do
      result <- expectRight t $ Json.parse "null"
      assertEq t result Json.Null

    it t "parses true" $ do
      result <- expectRight t $ Json.parse "true"
      assertEq t result $ Json.Boolean True

    it t "parses false" $ do
      result <- expectRight t $ Json.parse "false"
      assertEq t result $ Json.Boolean False

    it t "parses integer" $ do
      result <- expectRight t $ Json.parse "123"
      assertEq t result . Json.Number $ Decimal.MkDecimal 123 0

    it t "parses negative integer" $ do
      result <- expectRight t $ Json.parse "-42"
      assertEq t result . Json.Number $ Decimal.MkDecimal (-42) 0

    it t "parses decimal" $ do
      result <- expectRight t $ Json.parse "1.23"
      assertEq t result . Json.Number $ Decimal.MkDecimal 123 (-2)

    it t "parses scientific notation" $ do
      result <- expectRight t $ Json.parse "1e10"
      assertEq t result . Json.Number $ Decimal.MkDecimal 1 10

    it t "parses scientific with decimal" $ do
      result <- expectRight t $ Json.parse "1.5e2"
      assertEq t result . Json.Number $ Decimal.MkDecimal 15 1

    it t "parses string" $ do
      result <- expectRight t $ Json.parse "\"hello\""
      assertEq t result $ Json.String "hello"

    it t "parses string with escapes" $ do
      result <- expectRight t $ Json.parse "\"a\\nb\""
      assertEq t result $ Json.String "a\nb"

    it t "parses string with unicode escape" $ do
      result <- expectRight t $ Json.parse "\"\\u0041\""
      assertEq t result $ Json.String "A"

    it t "parses empty array" $ do
      result <- expectRight t $ Json.parse "[]"
      assertEq t result $ Json.Array []

    it t "parses empty object" $ do
      result <- expectRight t $ Json.parse "{}"
      assertEq t result $ Json.Object Map.empty

    it t "parses array with values" $ do
      result <- expectRight t $ Json.parse "[1, 2, 3]"
      assertEq t result $
        Json.Array
          [ Json.Number $ Decimal.MkDecimal 1 0,
            Json.Number $ Decimal.MkDecimal 2 0,
            Json.Number $ Decimal.MkDecimal 3 0
          ]

    it t "parses object with values" $ do
      result <- expectRight t $ Json.parse "{\"a\": 1}"
      assertEq t result . Json.Object $
        Map.singleton "a" (Json.Number $ Decimal.MkDecimal 1 0)

    it t "parses nested structures" $ do
      result <- expectRight t $ Json.parse "{\"arr\": [1, {\"nested\": true}]}"
      assertEq t result . Json.Object $
        Map.singleton
          "arr"
          ( Json.Array
              [ Json.Number $ Decimal.MkDecimal 1 0,
                Json.Object $ Map.singleton "nested" (Json.Boolean True)
              ]
          )

    it t "handles whitespace" $ do
      result <- expectRight t $ Json.parse "  { \"a\" : 1 }  "
      assertEq t result . Json.Object $
        Map.singleton "a" (Json.Number $ Decimal.MkDecimal 1 0)

    it t "fails on invalid json" $ do
      assertSatisfies t Either.isLeft $ Json.parse "invalid"

    it t "fails on trailing content" $ do
      assertSatisfies t Either.isLeft $ Json.parse "null extra"

  describe t "render" $ do
    it t "renders null" $ do
      assertEq t (Json.render Json.Null) "null"

    it t "renders true" $ do
      assertEq t (Json.render $ Json.Boolean True) "true"

    it t "renders false" $ do
      assertEq t (Json.render $ Json.Boolean False) "false"

    it t "renders number" $ do
      assertEq t (Json.render . Json.Number $ Decimal.MkDecimal 123 0) "123"

    it t "renders string" $ do
      assertEq t (Json.render $ Json.String "hello") "\"hello\""

    it t "renders string with escapes" $ do
      let rendered = Json.render $ Json.String "a\nb"
      assertEq t rendered "\"a\\nb\""

    it t "renders empty array" $ do
      assertEq t (Json.render $ Json.Array []) "[]"

    it t "renders array" $ do
      let rendered = Json.render $ Json.Array [Json.Null, Json.Boolean True]
      assertEq t rendered "[null,true]"

    it t "renders empty object" $ do
      assertEq t (Json.render $ Json.Object Map.empty) "{}"

    it t "renders object" $ do
      let rendered = Json.render . Json.Object $ Map.singleton "a" Json.Null
      assertEq t rendered "{\"a\":null}"

  describe t "roundtrip" $ do
    it t "null roundtrips" $ do
      let original = Json.Null
      let rendered = Json.render original
      result <- expectRight t . Json.parse $ decodeUtf8 rendered
      assertEq t result original

    it t "boolean roundtrips" $ do
      let original = Json.Boolean True
      let rendered = Json.render original
      result <- expectRight t . Json.parse $ decodeUtf8 rendered
      assertEq t result original

    it t "number roundtrips" $ do
      let original = Json.Number $ Decimal.MkDecimal 12345 (-2)
      let rendered = Json.render original
      result <- expectRight t . Json.parse $ decodeUtf8 rendered
      assertEq t result original

    it t "string roundtrips" $ do
      let original = Json.String "hello world"
      let rendered = Json.render original
      result <- expectRight t . Json.parse $ decodeUtf8 rendered
      assertEq t result original

    it t "array roundtrips" $ do
      let original = Json.Array [Json.Null, Json.Boolean True, Json.Number $ Decimal.MkDecimal 1 0]
      let rendered = Json.render original
      result <- expectRight t . Json.parse $ decodeUtf8 rendered
      assertEq t result original

    it t "object roundtrips" $ do
      let original =
            Json.Object $
              Map.fromList
                [ ("name", Json.String "test"),
                  ("count", Json.Number $ Decimal.MkDecimal 42 0)
                ]
      let rendered = Json.render original
      result <- expectRight t . Json.parse $ decodeUtf8 rendered
      assertEq t result original

    it t "nested structure roundtrips" $ do
      let original =
            Json.Object $
              Map.singleton
                "data"
                ( Json.Array
                    [ Json.Object $ Map.singleton "id" (Json.Number $ Decimal.MkDecimal 1 0),
                      Json.Object $ Map.singleton "id" (Json.Number $ Decimal.MkDecimal 2 0)
                    ]
                )
      let rendered = Json.render original
      result <- expectRight t . Json.parse $ decodeUtf8 rendered
      assertEq t result original

decodeUtf8 :: LazyByteString.ByteString -> Text.Text
decodeUtf8 = Encoding.decodeUtf8Lenient . LazyByteString.toStrict
