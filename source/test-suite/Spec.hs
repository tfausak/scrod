{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 #-}

module Spec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Either as Either
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector
import qualified Scrod
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (testCase, (@?=))

spec :: [Tasty.TestTree]
spec =
  [ Tasty.testGroup
      "json"
      [ Tasty.testGroup
          "parse"
          [ testCase "parses null" $ do
              Right result <- pure $ Scrod.parse "null"
              result @?= Aeson.Null,
            testCase "parses true" $ do
              Right result <- pure $ Scrod.parse "true"
              result @?= Aeson.Bool True,
            testCase "parses false" $ do
              Right result <- pure $ Scrod.parse "false"
              result @?= Aeson.Bool False,
            testCase "parses integer" $ do
              Right result <- pure $ Scrod.parse "123"
              result @?= Aeson.Number (Scientific.scientific 123 0),
            testCase "parses negative integer" $ do
              Right result <- pure $ Scrod.parse "-42"
              result @?= Aeson.Number (Scientific.scientific (-42) 0),
            testCase "parses decimal" $ do
              Right result <- pure $ Scrod.parse "1.23"
              result @?= Aeson.Number (Scientific.scientific 123 (-2)),
            testCase "parses scientific notation" $ do
              Right result <- pure $ Scrod.parse "1e10"
              result @?= Aeson.Number (Scientific.scientific 1 10),
            testCase "parses scientific with decimal" $ do
              Right result <- pure $ Scrod.parse "1.5e2"
              result @?= Aeson.Number (Scientific.scientific 15 1),
            testCase "parses string" $ do
              Right result <- pure $ Scrod.parse "\"hello\""
              result @?= Aeson.String "hello",
            testCase "parses string with escapes" $ do
              Right result <- pure $ Scrod.parse "\"a\\nb\""
              result @?= Aeson.String "a\nb",
            testCase "parses string with unicode escape" $ do
              Right result <- pure $ Scrod.parse "\"\\u0041\""
              result @?= Aeson.String "A",
            testCase "parses empty array" $ do
              Right result <- pure $ Scrod.parse "[]"
              result @?= Aeson.Array Vector.empty,
            testCase "parses empty object" $ do
              Right result <- pure $ Scrod.parse "{}"
              result @?= Aeson.Object KeyMap.empty,
            testCase "parses array with values" $ do
              Right result <- pure $ Scrod.parse "[1, 2, 3]"
              result
                @?= Aeson.Array
                  ( Vector.fromList
                      [ Aeson.Number $ Scientific.scientific 1 0,
                        Aeson.Number $ Scientific.scientific 2 0,
                        Aeson.Number $ Scientific.scientific 3 0
                      ]
                  ),
            testCase "parses object with values" $ do
              Right result <- pure $ Scrod.parse "{\"a\": 1}"
              result
                @?= Aeson.Object
                  (KeyMap.singleton "a" (Aeson.Number $ Scientific.scientific 1 0)),
            testCase "parses nested structures" $ do
              Right result <- pure $ Scrod.parse "{\"arr\": [1, {\"nested\": true}]}"
              result
                @?= Aeson.Object
                  ( KeyMap.singleton
                      "arr"
                      ( Aeson.Array $
                          Vector.fromList
                            [ Aeson.Number $ Scientific.scientific 1 0,
                              Aeson.Object $ KeyMap.singleton "nested" (Aeson.Bool True)
                            ]
                      )
                  ),
            testCase "handles whitespace" $ do
              Right result <- pure $ Scrod.parse "  { \"a\" : 1 }  "
              result
                @?= Aeson.Object
                  (KeyMap.singleton "a" (Aeson.Number $ Scientific.scientific 1 0)),
            testCase "fails on invalid json" $
              Either.isLeft (Scrod.parse "invalid") @?= True,
            testCase "fails on trailing content" $
              Either.isLeft (Scrod.parse "null extra") @?= True
          ],
        Tasty.testGroup
          "render"
          [ testCase "renders null" $
              Scrod.render Aeson.Null @?= "null",
            testCase "renders true" $
              Scrod.render (Aeson.Bool True) @?= "true",
            testCase "renders false" $
              Scrod.render (Aeson.Bool False) @?= "false",
            testCase "renders number" $
              Scrod.render (Aeson.Number $ Scientific.scientific 123 0) @?= "123",
            testCase "renders string" $
              Scrod.render (Aeson.String "hello") @?= "\"hello\"",
            testCase "renders string with escapes" $
              Scrod.render (Aeson.String "a\nb") @?= "\"a\\nb\"",
            testCase "renders empty array" $
              Scrod.render (Aeson.Array Vector.empty) @?= "[]",
            testCase "renders array" $
              Scrod.render (Aeson.Array $ Vector.fromList [Aeson.Null, Aeson.Bool True]) @?= "[null,true]",
            testCase "renders empty object" $
              Scrod.render (Aeson.Object KeyMap.empty) @?= "{}",
            testCase "renders object" $
              Scrod.render (Aeson.Object $ KeyMap.singleton "a" Aeson.Null) @?= "{\"a\":null}"
          ],
        Tasty.testGroup
          "roundtrip"
          [ testCase "null roundtrips" $ do
              let original = Aeson.Null
              let rendered = Scrod.render original
              Right result <- pure . Scrod.parse $ decodeUtf8 rendered
              result @?= original,
            testCase "boolean roundtrips" $ do
              let original = Aeson.Bool True
              let rendered = Scrod.render original
              Right result <- pure . Scrod.parse $ decodeUtf8 rendered
              result @?= original,
            testCase "number roundtrips" $ do
              let original = Aeson.Number $ Scientific.scientific 12345 (-2)
              let rendered = Scrod.render original
              Right result <- pure . Scrod.parse $ decodeUtf8 rendered
              result @?= original,
            testCase "string roundtrips" $ do
              let original = Aeson.String "hello world"
              let rendered = Scrod.render original
              Right result <- pure . Scrod.parse $ decodeUtf8 rendered
              result @?= original,
            testCase "array roundtrips" $ do
              let original = Aeson.Array $ Vector.fromList [Aeson.Null, Aeson.Bool True, Aeson.Number $ Scientific.scientific 1 0]
              let rendered = Scrod.render original
              Right result <- pure . Scrod.parse $ decodeUtf8 rendered
              result @?= original,
            testCase "object roundtrips" $ do
              let original =
                    Aeson.Object $
                      KeyMap.fromList
                        [ ("name", Aeson.String "test"),
                          ("count", Aeson.Number $ Scientific.scientific 42 0)
                        ]
              let rendered = Scrod.render original
              Right result <- pure . Scrod.parse $ decodeUtf8 rendered
              result @?= original,
            testCase "nested structure roundtrips" $ do
              let original =
                    Aeson.Object $
                      KeyMap.singleton
                        "data"
                        ( Aeson.Array $
                            Vector.fromList
                              [ Aeson.Object $ KeyMap.singleton "id" (Aeson.Number $ Scientific.scientific 1 0),
                                Aeson.Object $ KeyMap.singleton "id" (Aeson.Number $ Scientific.scientific 2 0)
                              ]
                        )
              let rendered = Scrod.render original
              Right result <- pure . Scrod.parse $ decodeUtf8 rendered
              result @?= original
          ]
      ]
  ]

decodeUtf8 :: LazyByteString.ByteString -> Text.Text
decodeUtf8 = Encoding.decodeUtf8Lenient . LazyByteString.toStrict
