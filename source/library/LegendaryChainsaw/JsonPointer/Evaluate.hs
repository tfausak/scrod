{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.JsonPointer.Evaluate where

import qualified Data.Function as Function
import Data.List ((!?))
import qualified Data.List as List
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Decimal as Decimal
import qualified LegendaryChainsaw.Extra.Read as Read
import qualified LegendaryChainsaw.Json.Array as Array
import qualified LegendaryChainsaw.Json.Null as Null
import qualified LegendaryChainsaw.Json.Number as Number
import qualified LegendaryChainsaw.Json.Object as Object
import qualified LegendaryChainsaw.Json.Pair as Pair
import qualified LegendaryChainsaw.Json.String as String
import qualified LegendaryChainsaw.Json.Value as Value
import qualified LegendaryChainsaw.JsonPointer.Pointer as Pointer
import qualified LegendaryChainsaw.JsonPointer.Token as Token
import qualified LegendaryChainsaw.Spec as Spec

-- | Evaluates a JSON Pointer against a JSON Value.
-- Returns Nothing if the path does not exist or is invalid.
-- Per RFC 6901:
-- - An empty pointer returns the document itself
-- - For arrays, tokens must be valid non-negative integer indices
-- - For objects, tokens are matched against member names
evaluate :: Pointer.Pointer -> Value.Value -> Maybe Value.Value
evaluate = Function.fix $ \rec pointer value ->
  case Pointer.unwrap pointer of
    [] -> Just value
    token : rest -> do
      child <- step token value
      rec (Pointer.MkPointer rest) child

-- | Takes a single step in a JSON value using a reference token.
step :: Token.Token -> Value.Value -> Maybe Value.Value
step token value = case value of
  Value.Array array -> stepArray token array
  Value.Object object -> stepObject token object
  _ -> Nothing

-- | Steps into an array using a token as an index.
-- Per RFC 6901, array indices must be:
-- - Non-negative integers
-- - Either "0" or not starting with "0" (no leading zeros)
stepArray :: Token.Token -> Array.Array a -> Maybe a
stepArray token array = do
  index <- case Text.unpack $ Token.unwrap token of
    '0' : _ : _ -> Nothing
    string -> Read.readM string
  Array.unwrap array !? index

-- | Steps into an object using a token as a key.
stepObject :: Token.Token -> Object.Object a -> Maybe a
stepObject token =
  fmap Pair.value
    . List.find ((== Token.unwrap token) . String.unwrap . Pair.name)
    . Object.unwrap

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  let null_ = Value.Null Null.MkNull
      number m = Value.Number . Number.MkNumber . Decimal.mkDecimal m
      string = Value.String . String.MkString . Text.pack
      array = Value.Array . Array.MkArray
      object = Value.Object . Object.MkObject . fmap (\(n, v) -> Pair.MkPair (String.MkString $ Text.pack n) v)
      pointer = Pointer.MkPointer . fmap (Token.MkToken . Text.pack)

  Spec.named s 'evaluate $ do
    Spec.it s "empty pointer returns the document" $ do
      Spec.assertEq s (evaluate (pointer []) null_) $ Just null_

    Spec.it s "empty pointer returns the document (object)" $ do
      let doc = object [("foo", number 1 0)]
      Spec.assertEq s (evaluate (pointer []) doc) $ Just doc

    Spec.it s "returns object member by name" $ do
      let doc = object [("foo", string "bar")]
      Spec.assertEq s (evaluate (pointer ["foo"]) doc) . Just $ string "bar"

    Spec.it s "returns nested object member" $ do
      let doc = object [("foo", object [("bar", number 42 0)])]
      Spec.assertEq s (evaluate (pointer ["foo", "bar"]) doc) . Just $ number 42 0

    Spec.it s "returns array element by index" $ do
      let doc = array [string "a", string "b", string "c"]
      Spec.assertEq s (evaluate (pointer ["1"]) doc) . Just $ string "b"

    Spec.it s "returns first array element with index 0" $ do
      let doc = array [string "first", string "second"]
      Spec.assertEq s (evaluate (pointer ["0"]) doc) . Just $ string "first"

    Spec.it s "returns Nothing for out-of-bounds array index" $ do
      let doc = array [string "a"]
      Spec.assertEq s (evaluate (pointer ["5"]) doc) Nothing

    Spec.it s "returns Nothing for non-integer array index" $ do
      let doc = array [string "a"]
      Spec.assertEq s (evaluate (pointer ["foo"]) doc) Nothing

    Spec.it s "returns Nothing for leading zero in array index" $ do
      let doc = array [string "a", string "b"]
      Spec.assertEq s (evaluate (pointer ["01"]) doc) Nothing

    Spec.it s "returns Nothing for negative array index" $ do
      let doc = array [string "a", string "b"]
      Spec.assertEq s (evaluate (pointer ["-1"]) doc) Nothing

    Spec.it s "returns Nothing for missing object key" $ do
      let doc = object [("foo", string "bar")]
      Spec.assertEq s (evaluate (pointer ["baz"]) doc) Nothing

    Spec.it s "returns Nothing when stepping into a scalar" $ do
      Spec.assertEq s (evaluate (pointer ["foo"]) (string "bar")) Nothing

    -- RFC 6901 Section 5 examples
    let rfc6901Doc =
          object
            [ ("foo", array [string "bar", string "baz"]),
              ("", number 0 0),
              ("a/b", number 1 0),
              ("c%d", number 2 0),
              ("e^f", number 3 0),
              ("g|h", number 4 0),
              ("i\\j", number 5 0),
              ("k\"l", number 6 0),
              (" ", number 7 0),
              ("m~n", number 8 0)
            ]

    Spec.it s "RFC 6901: empty pointer returns the whole document" $ do
      Spec.assertEq s (evaluate (pointer []) rfc6901Doc) $ Just rfc6901Doc

    Spec.it s "RFC 6901: /foo returns [\"bar\", \"baz\"]" $ do
      Spec.assertEq s (evaluate (pointer ["foo"]) rfc6901Doc) . Just $ array [string "bar", string "baz"]

    Spec.it s "RFC 6901: /foo/0 returns \"bar\"" $ do
      Spec.assertEq s (evaluate (pointer ["foo", "0"]) rfc6901Doc) . Just $ string "bar"

    Spec.it s "RFC 6901: / (empty token) returns 0" $ do
      Spec.assertEq s (evaluate (pointer [""]) rfc6901Doc) . Just $ number 0 0

    Spec.it s "RFC 6901: /a~1b (unescaped a/b) returns 1" $ do
      Spec.assertEq s (evaluate (pointer ["a/b"]) rfc6901Doc) . Just $ number 1 0

    Spec.it s "RFC 6901: /c%d returns 2" $ do
      Spec.assertEq s (evaluate (pointer ["c%d"]) rfc6901Doc) . Just $ number 2 0

    Spec.it s "RFC 6901: /e^f returns 3" $ do
      Spec.assertEq s (evaluate (pointer ["e^f"]) rfc6901Doc) . Just $ number 3 0

    Spec.it s "RFC 6901: /g|h returns 4" $ do
      Spec.assertEq s (evaluate (pointer ["g|h"]) rfc6901Doc) . Just $ number 4 0

    Spec.it s "RFC 6901: /i\\j returns 5" $ do
      Spec.assertEq s (evaluate (pointer ["i\\j"]) rfc6901Doc) . Just $ number 5 0

    Spec.it s "RFC 6901: /k\"l returns 6" $ do
      Spec.assertEq s (evaluate (pointer ["k\"l"]) rfc6901Doc) . Just $ number 6 0

    Spec.it s "RFC 6901: / (space) returns 7" $ do
      Spec.assertEq s (evaluate (pointer [" "]) rfc6901Doc) . Just $ number 7 0

    Spec.it s "RFC 6901: /m~0n (unescaped m~n) returns 8" $ do
      Spec.assertEq s (evaluate (pointer ["m~n"]) rfc6901Doc) . Just $ number 8 0

    Spec.it s "handles empty string key" $ do
      let doc = object [("", string "empty key")]
      Spec.assertEq s (evaluate (pointer [""]) doc) . Just $ string "empty key"

    Spec.it s "handles deeply nested path" $ do
      let doc = object [("a", object [("b", object [("c", string "deep")])])]
      Spec.assertEq s (evaluate (pointer ["a", "b", "c"]) doc) . Just $ string "deep"

    Spec.it s "handles mixed array and object traversal" $ do
      let doc = object [("items", array [object [("name", string "first")], object [("name", string "second")]])]
      Spec.assertEq s (evaluate (pointer ["items", "1", "name"]) doc) . Just $ string "second"
