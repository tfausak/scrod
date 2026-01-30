{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 #-}

module Spec (spec) where

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Either as Either
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector
import qualified Scrod.Type.Json as Json
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (testCase, (@?=))

spec :: [Tasty.TestTree]
spec =
  [ Tasty.testGroup
      "json"
      [ Tasty.testGroup
          "parse"
          [ testCase "parses null" $ do
              Right result <- pure $ Json.parse "null"
              result @?= Json.Null,
            testCase "parses true" $ do
              Right result <- pure $ Json.parse "true"
              result @?= Json.Boolean True,
            testCase "parses false" $ do
              Right result <- pure $ Json.parse "false"
              result @?= Json.Boolean False,
            testCase "parses integer" $ do
              Right result <- pure $ Json.parse "123"
              result @?= Json.Number (Scientific.scientific 123 0),
            testCase "parses negative integer" $ do
              Right result <- pure $ Json.parse "-42"
              result @?= Json.Number (Scientific.scientific (-42) 0),
            testCase "parses decimal" $ do
              Right result <- pure $ Json.parse "1.23"
              result @?= Json.Number (Scientific.scientific 123 (-2)),
            testCase "parses scientific notation" $ do
              Right result <- pure $ Json.parse "1e10"
              result @?= Json.Number (Scientific.scientific 1 10),
            testCase "parses scientific with decimal" $ do
              Right result <- pure $ Json.parse "1.5e2"
              result @?= Json.Number (Scientific.scientific 15 1),
            testCase "parses string" $ do
              Right result <- pure $ Json.parse "\"hello\""
              result @?= Json.String "hello",
            testCase "parses string with escapes" $ do
              Right result <- pure $ Json.parse "\"a\\nb\""
              result @?= Json.String "a\nb",
            testCase "parses string with unicode escape" $ do
              Right result <- pure $ Json.parse "\"\\u0041\""
              result @?= Json.String "A",
            testCase "parses empty array" $ do
              Right result <- pure $ Json.parse "[]"
              result @?= Json.Array Vector.empty,
            testCase "parses empty object" $ do
              Right result <- pure $ Json.parse "{}"
              result @?= Json.Object KeyMap.empty,
            testCase "parses array with values" $ do
              Right result <- pure $ Json.parse "[1, 2, 3]"
              result
                @?= Json.Array
                  ( Vector.fromList
                      [ Json.Number $ Scientific.scientific 1 0,
                        Json.Number $ Scientific.scientific 2 0,
                        Json.Number $ Scientific.scientific 3 0
                      ]
                  ),
            testCase "parses object with values" $ do
              Right result <- pure $ Json.parse "{\"a\": 1}"
              result
                @?= Json.Object
                  (KeyMap.singleton "a" (Json.Number $ Scientific.scientific 1 0)),
            testCase "parses nested structures" $ do
              Right result <- pure $ Json.parse "{\"arr\": [1, {\"nested\": true}]}"
              result
                @?= Json.Object
                  ( KeyMap.singleton
                      "arr"
                      ( Json.Array $
                          Vector.fromList
                            [ Json.Number $ Scientific.scientific 1 0,
                              Json.Object $ KeyMap.singleton "nested" (Json.Boolean True)
                            ]
                      )
                  ),
            testCase "handles whitespace" $ do
              Right result <- pure $ Json.parse "  { \"a\" : 1 }  "
              result
                @?= Json.Object
                  (KeyMap.singleton "a" (Json.Number $ Scientific.scientific 1 0)),
            testCase "fails on invalid json" $
              Either.isLeft (Json.parse "invalid") @?= True,
            testCase "fails on trailing content" $
              Either.isLeft (Json.parse "null extra") @?= True
          ],
        Tasty.testGroup
          "render"
          [ testCase "renders null" $
              Json.render Json.Null @?= "null",
            testCase "renders true" $
              Json.render (Json.Boolean True) @?= "true",
            testCase "renders false" $
              Json.render (Json.Boolean False) @?= "false",
            testCase "renders number" $
              Json.render (Json.Number $ Scientific.scientific 123 0) @?= "123",
            testCase "renders string" $
              Json.render (Json.String "hello") @?= "\"hello\"",
            testCase "renders string with escapes" $
              Json.render (Json.String "a\nb") @?= "\"a\\nb\"",
            testCase "renders empty array" $
              Json.render (Json.Array Vector.empty) @?= "[]",
            testCase "renders array" $
              Json.render (Json.Array $ Vector.fromList [Json.Null, Json.Boolean True]) @?= "[null,true]",
            testCase "renders empty object" $
              Json.render (Json.Object KeyMap.empty) @?= "{}",
            testCase "renders object" $
              Json.render (Json.Object $ KeyMap.singleton "a" Json.Null) @?= "{\"a\":null}"
          ],
        Tasty.testGroup
          "roundtrip"
          [ testCase "null roundtrips" $ do
              let original = Json.Null
              let rendered = Json.render original
              Right result <- pure . Json.parse $ decodeUtf8 rendered
              result @?= original,
            testCase "boolean roundtrips" $ do
              let original = Json.Boolean True
              let rendered = Json.render original
              Right result <- pure . Json.parse $ decodeUtf8 rendered
              result @?= original,
            testCase "number roundtrips" $ do
              let original = Json.Number $ Scientific.scientific 12345 (-2)
              let rendered = Json.render original
              Right result <- pure . Json.parse $ decodeUtf8 rendered
              result @?= original,
            testCase "string roundtrips" $ do
              let original = Json.String "hello world"
              let rendered = Json.render original
              Right result <- pure . Json.parse $ decodeUtf8 rendered
              result @?= original,
            testCase "array roundtrips" $ do
              let original = Json.Array $ Vector.fromList [Json.Null, Json.Boolean True, Json.Number $ Scientific.scientific 1 0]
              let rendered = Json.render original
              Right result <- pure . Json.parse $ decodeUtf8 rendered
              result @?= original,
            testCase "object roundtrips" $ do
              let original =
                    Json.Object $
                      KeyMap.fromList
                        [ ("name", Json.String "test"),
                          ("count", Json.Number $ Scientific.scientific 42 0)
                        ]
              let rendered = Json.render original
              Right result <- pure . Json.parse $ decodeUtf8 rendered
              result @?= original,
            testCase "nested structure roundtrips" $ do
              let original =
                    Json.Object $
                      KeyMap.singleton
                        "data"
                        ( Json.Array $
                            Vector.fromList
                              [ Json.Object $ KeyMap.singleton "id" (Json.Number $ Scientific.scientific 1 0),
                                Json.Object $ KeyMap.singleton "id" (Json.Number $ Scientific.scientific 2 0)
                              ]
                        )
              let rendered = Json.render original
              Right result <- pure . Json.parse $ decodeUtf8 rendered
              result @?= original
          ]
      ]
  ]

decodeUtf8 :: LazyByteString.ByteString -> Text.Text
decodeUtf8 = Encoding.decodeUtf8Lenient . LazyByteString.toStrict
