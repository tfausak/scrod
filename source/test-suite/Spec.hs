{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 #-}

module Spec (spec) where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Scrod.Unstable.Type.Decimal as Decimal
import qualified Scrod.Unstable.Type.Json as Json
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Unit

spec :: [Tasty.TestTree]
spec =
  [ Tasty.testGroup
      "json"
      [ Tasty.testGroup
          "parse"
          [ Unit.testCase "parses null" $ do
              result <- expectRight $ Json.parse "null"
              result Unit.@?= Json.Null,
            Unit.testCase "parses true" $ do
              result <- expectRight $ Json.parse "true"
              result Unit.@?= Json.Boolean True,
            Unit.testCase "parses false" $ do
              result <- expectRight $ Json.parse "false"
              result Unit.@?= Json.Boolean False,
            Unit.testCase "parses integer" $ do
              result <- expectRight $ Json.parse "123"
              result Unit.@?= Json.Number (Decimal.MkDecimal 123 0),
            Unit.testCase "parses negative integer" $ do
              result <- expectRight $ Json.parse "-42"
              result Unit.@?= Json.Number (Decimal.MkDecimal (-42) 0),
            Unit.testCase "parses decimal" $ do
              result <- expectRight $ Json.parse "1.23"
              result Unit.@?= Json.Number (Decimal.MkDecimal 123 (-2)),
            Unit.testCase "parses scientific notation" $ do
              result <- expectRight $ Json.parse "1e10"
              result Unit.@?= Json.Number (Decimal.MkDecimal 1 10),
            Unit.testCase "parses scientific with decimal" $ do
              result <- expectRight $ Json.parse "1.5e2"
              result Unit.@?= Json.Number (Decimal.MkDecimal 15 1),
            Unit.testCase "parses string" $ do
              result <- expectRight $ Json.parse "\"hello\""
              result Unit.@?= Json.String "hello",
            Unit.testCase "parses string with escapes" $ do
              result <- expectRight $ Json.parse "\"a\\nb\""
              result Unit.@?= Json.String "a\nb",
            Unit.testCase "parses string with unicode escape" $ do
              result <- expectRight $ Json.parse "\"\\u0041\""
              result Unit.@?= Json.String "A",
            Unit.testCase "parses empty array" $ do
              result <- expectRight $ Json.parse "[]"
              result Unit.@?= Json.Array [],
            Unit.testCase "parses empty object" $ do
              result <- expectRight $ Json.parse "{}"
              result Unit.@?= Json.Object Map.empty,
            Unit.testCase "parses array with values" $ do
              result <- expectRight $ Json.parse "[1, 2, 3]"
              result
                Unit.@?= Json.Array
                  [ Json.Number $ Decimal.MkDecimal 1 0,
                    Json.Number $ Decimal.MkDecimal 2 0,
                    Json.Number $ Decimal.MkDecimal 3 0
                  ],
            Unit.testCase "parses object with values" $ do
              result <- expectRight $ Json.parse "{\"a\": 1}"
              result
                Unit.@?= Json.Object
                  (Map.singleton "a" (Json.Number $ Decimal.MkDecimal 1 0)),
            Unit.testCase "parses nested structures" $ do
              result <- expectRight $ Json.parse "{\"arr\": [1, {\"nested\": true}]}"
              result
                Unit.@?= Json.Object
                  ( Map.singleton
                      "arr"
                      ( Json.Array
                          [ Json.Number $ Decimal.MkDecimal 1 0,
                            Json.Object $ Map.singleton "nested" (Json.Boolean True)
                          ]
                      )
                  ),
            Unit.testCase "handles whitespace" $ do
              result <- expectRight $ Json.parse "  { \"a\" : 1 }  "
              result
                Unit.@?= Json.Object
                  (Map.singleton "a" (Json.Number $ Decimal.MkDecimal 1 0)),
            Unit.testCase "fails on invalid json" $
              Either.isLeft (Json.parse "invalid") Unit.@?= True,
            Unit.testCase "fails on trailing content" $
              Either.isLeft (Json.parse "null extra") Unit.@?= True
          ],
        Tasty.testGroup
          "render"
          [ Unit.testCase "renders null" $
              Json.render Json.Null Unit.@?= "null",
            Unit.testCase "renders true" $
              Json.render (Json.Boolean True) Unit.@?= "true",
            Unit.testCase "renders false" $
              Json.render (Json.Boolean False) Unit.@?= "false",
            Unit.testCase "renders number" $
              Json.render (Json.Number $ Decimal.MkDecimal 123 0) Unit.@?= "123",
            Unit.testCase "renders string" $
              Json.render (Json.String "hello") Unit.@?= "\"hello\"",
            Unit.testCase "renders string with escapes" $
              Json.render (Json.String "a\nb") Unit.@?= "\"a\\nb\"",
            Unit.testCase "renders empty array" $
              Json.render (Json.Array []) Unit.@?= "[]",
            Unit.testCase "renders array" $
              Json.render (Json.Array [Json.Null, Json.Boolean True]) Unit.@?= "[null,true]",
            Unit.testCase "renders empty object" $
              Json.render (Json.Object Map.empty) Unit.@?= "{}",
            Unit.testCase "renders object" $
              Json.render (Json.Object $ Map.singleton "a" Json.Null) Unit.@?= "{\"a\":null}"
          ],
        Tasty.testGroup
          "roundtrip"
          [ Unit.testCase "null roundtrips" $ do
              let original = Json.Null
              let rendered = Json.render original
              result <- expectRight . Json.parse $ decodeUtf8 rendered
              result Unit.@?= original,
            Unit.testCase "boolean roundtrips" $ do
              let original = Json.Boolean True
              let rendered = Json.render original
              result <- expectRight . Json.parse $ decodeUtf8 rendered
              result Unit.@?= original,
            Unit.testCase "number roundtrips" $ do
              let original = Json.Number $ Decimal.MkDecimal 12345 (-2)
              let rendered = Json.render original
              result <- expectRight . Json.parse $ decodeUtf8 rendered
              result Unit.@?= original,
            Unit.testCase "string roundtrips" $ do
              let original = Json.String "hello world"
              let rendered = Json.render original
              result <- expectRight . Json.parse $ decodeUtf8 rendered
              result Unit.@?= original,
            Unit.testCase "array roundtrips" $ do
              let original = Json.Array [Json.Null, Json.Boolean True, Json.Number $ Decimal.MkDecimal 1 0]
              let rendered = Json.render original
              result <- expectRight . Json.parse $ decodeUtf8 rendered
              result Unit.@?= original,
            Unit.testCase "object roundtrips" $ do
              let original =
                    Json.Object $
                      Map.fromList
                        [ ("name", Json.String "test"),
                          ("count", Json.Number $ Decimal.MkDecimal 42 0)
                        ]
              let rendered = Json.render original
              result <- expectRight . Json.parse $ decodeUtf8 rendered
              result Unit.@?= original,
            Unit.testCase "nested structure roundtrips" $ do
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
              result <- expectRight . Json.parse $ decodeUtf8 rendered
              result Unit.@?= original
          ]
      ]
  ]

decodeUtf8 :: LazyByteString.ByteString -> Text.Text
decodeUtf8 = Encoding.decodeUtf8Lenient . LazyByteString.toStrict

expectRight :: (Show a) => Either a b -> IO b
expectRight e = case e of
  Left x -> Unit.assertFailure $ "expected Right but got Left " <> show x
  Right r -> pure r
