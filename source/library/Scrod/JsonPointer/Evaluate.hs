{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.JsonPointer.Evaluate where

import qualified Data.Function as Function
import Data.List ((!?))
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Scrod.Extra.Read as Read
import qualified Scrod.Json.Array as Array
import qualified Scrod.Json.Object as Object
import qualified Scrod.Json.Pair as Pair
import qualified Scrod.Json.String as String
import qualified Scrod.Json.Value as Value
import qualified Scrod.JsonPointer.Pointer as Pointer
import qualified Scrod.JsonPointer.Token as Token
import qualified Scrod.Spec as Spec

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
  Spec.named s 'evaluate $ do
    Spec.it s "empty pointer returns the document" $ do
      Spec.assertEq s (evaluate (Pointer.pointer []) Value.null) $ Just Value.null

    Spec.it s "empty pointer returns the document (object)" $ do
      let doc = Value.object [("foo", Value.integer 1)]
      Spec.assertEq s (evaluate (Pointer.pointer []) doc) $ Just doc

    Spec.it s "returns object member by name" $ do
      let doc = Value.object [("foo", Value.string "bar")]
      Spec.assertEq s (evaluate (Pointer.pointer ["foo"]) doc) . Just $ Value.string "bar"

    Spec.it s "returns nested object member" $ do
      let doc = Value.object [("foo", Value.object [("bar", Value.integer 42)])]
      Spec.assertEq s (evaluate (Pointer.pointer ["foo", "bar"]) doc) . Just $ Value.integer 42

    Spec.it s "returns array element by index" $ do
      let doc = Value.array [Value.string "a", Value.string "b", Value.string "c"]
      Spec.assertEq s (evaluate (Pointer.pointer ["1"]) doc) . Just $ Value.string "b"

    Spec.it s "returns first array element with index 0" $ do
      let doc = Value.array [Value.string "first", Value.string "second"]
      Spec.assertEq s (evaluate (Pointer.pointer ["0"]) doc) . Just $ Value.string "first"

    Spec.it s "returns Nothing for out-of-bounds array index" $ do
      let doc = Value.array [Value.string "a"]
      Spec.assertEq s (evaluate (Pointer.pointer ["5"]) doc) Nothing

    Spec.it s "returns Nothing for non-integer array index" $ do
      let doc = Value.array [Value.string "a"]
      Spec.assertEq s (evaluate (Pointer.pointer ["foo"]) doc) Nothing

    Spec.it s "returns Nothing for leading zero in array index" $ do
      let doc = Value.array [Value.string "a", Value.string "b"]
      Spec.assertEq s (evaluate (Pointer.pointer ["01"]) doc) Nothing

    Spec.it s "returns Nothing for negative array index" $ do
      let doc = Value.array [Value.string "a", Value.string "b"]
      Spec.assertEq s (evaluate (Pointer.pointer ["-1"]) doc) Nothing

    Spec.it s "returns Nothing for missing object key" $ do
      let doc = Value.object [("foo", Value.string "bar")]
      Spec.assertEq s (evaluate (Pointer.pointer ["baz"]) doc) Nothing

    Spec.it s "returns Nothing when stepping into a scalar" $ do
      Spec.assertEq s (evaluate (Pointer.pointer ["foo"]) (Value.string "bar")) Nothing

    Spec.describe s "rfc 6901 section 5" $ do
      let rfc6901Doc =
            Value.object
              [ ("foo", Value.array [Value.string "bar", Value.string "baz"]),
                ("", Value.integer 0),
                ("a/b", Value.integer 1),
                ("c%d", Value.integer 2),
                ("e^f", Value.integer 3),
                ("g|h", Value.integer 4),
                ("i\\j", Value.integer 5),
                ("k\"l", Value.integer 6),
                (" ", Value.integer 7),
                ("m~n", Value.integer 8)
              ]

      Spec.it s "empty pointer returns the whole document" $ do
        Spec.assertEq s (evaluate (Pointer.pointer []) rfc6901Doc) $ Just rfc6901Doc

      Spec.it s "/foo returns [\"bar\", \"baz\"]" $ do
        Spec.assertEq s (evaluate (Pointer.pointer ["foo"]) rfc6901Doc) . Just $ Value.array [Value.string "bar", Value.string "baz"]

      Spec.it s "/foo/0 returns \"bar\"" $ do
        Spec.assertEq s (evaluate (Pointer.pointer ["foo", "0"]) rfc6901Doc) . Just $ Value.string "bar"

      Spec.it s "/ (empty token) returns 0" $ do
        Spec.assertEq s (evaluate (Pointer.pointer [""]) rfc6901Doc) . Just $ Value.integer 0

      Spec.it s "/a~1b (unescaped a/b) returns 1" $ do
        Spec.assertEq s (evaluate (Pointer.pointer ["a/b"]) rfc6901Doc) . Just $ Value.integer 1

      Spec.it s "/c%d returns 2" $ do
        Spec.assertEq s (evaluate (Pointer.pointer ["c%d"]) rfc6901Doc) . Just $ Value.integer 2

      Spec.it s "/e^f returns 3" $ do
        Spec.assertEq s (evaluate (Pointer.pointer ["e^f"]) rfc6901Doc) . Just $ Value.integer 3

      Spec.it s "/g|h returns 4" $ do
        Spec.assertEq s (evaluate (Pointer.pointer ["g|h"]) rfc6901Doc) . Just $ Value.integer 4

      Spec.it s "/i\\j returns 5" $ do
        Spec.assertEq s (evaluate (Pointer.pointer ["i\\j"]) rfc6901Doc) . Just $ Value.integer 5

      Spec.it s "/k\"l returns 6" $ do
        Spec.assertEq s (evaluate (Pointer.pointer ["k\"l"]) rfc6901Doc) . Just $ Value.integer 6

      Spec.it s "/ (space) returns 7" $ do
        Spec.assertEq s (evaluate (Pointer.pointer [" "]) rfc6901Doc) . Just $ Value.integer 7

      Spec.it s "/m~0n (unescaped m~n) returns 8" $ do
        Spec.assertEq s (evaluate (Pointer.pointer ["m~n"]) rfc6901Doc) . Just $ Value.integer 8

    Spec.it s "handles empty string key" $ do
      let doc = Value.object [("", Value.string "empty key")]
      Spec.assertEq s (evaluate (Pointer.pointer [""]) doc) . Just $ Value.string "empty key"

    Spec.it s "handles deeply nested path" $ do
      let doc = Value.object [("a", Value.object [("b", Value.object [("c", Value.string "deep")])])]
      Spec.assertEq s (evaluate (Pointer.pointer ["a", "b", "c"]) doc) . Just $ Value.string "deep"

    Spec.it s "handles mixed array and object traversal" $ do
      let doc = Value.object [("items", Value.array [Value.object [("name", Value.string "first")], Value.object [("name", Value.string "second")]])]
      Spec.assertEq s (evaluate (Pointer.pointer ["items", "1", "name"]) doc) . Just $ Value.string "second"
