{-# LANGUAGE OverloadedStrings #-}

-- | JSON Pointer implementation per RFC 6901.
-- https://datatracker.ietf.org/doc/html/rfc6901
module Pointer
  ( -- * Types
    Pointer (..),
    Token (..),

    -- * Parsing
    parse,

    -- * Evaluation
    evaluate,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- | A JSON Pointer as described by RFC 6901.
-- A pointer is a sequence of reference tokens used to identify
-- a specific value within a JSON document.
newtype Pointer = MkPointer {tokens :: [Token]}
  deriving (Eq, Ord, Show)

-- | A single reference token in a JSON Pointer.
-- After parsing, escape sequences have been resolved.
newtype Token = MkToken {value :: Text.Text}
  deriving (Eq, Ord, Show)

-- | Parse a JSON Pointer from a string.
-- Per RFC 6901, a valid pointer is either empty or starts with '/'.
-- Returns Nothing for invalid pointers.
parse :: String -> Maybe Pointer
parse "" = Just $ MkPointer []
parse ('/' : rest) =
  Just . MkPointer . fmap (MkToken . unescape . Text.pack) $ splitOn '/' rest
parse _ = Nothing

-- | Split a string on a delimiter, keeping empty segments.
-- "/a/b" split on '/' gives ["a", "b"]
-- "/a//b" split on '/' gives ["a", "", "b"]
splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn c s = case break (== c) s of
  (before, "") -> [before]
  (before, _ : after) -> before : splitOn c after

-- | Unescape a JSON Pointer token per RFC 6901 section 4.
-- Order matters: ~0 must be processed last so that ~01 becomes ~1, not /.
-- ~1 -> /
-- ~0 -> ~
unescape :: Text.Text -> Text.Text
unescape =
  Text.replace "~0" "~"
    . Text.replace "~1" "/"

-- | Evaluate a JSON Pointer against a JSON value.
-- Returns Nothing if the pointer doesn't resolve to a value.
evaluate :: Pointer -> Aeson.Value -> Maybe Aeson.Value
evaluate (MkPointer []) json = Just json
evaluate (MkPointer (t : ts)) json = do
  next <- step t json
  evaluate (MkPointer ts) next

-- | Take a single step through a JSON value using a reference token.
step :: Token -> Aeson.Value -> Maybe Aeson.Value
step (MkToken key) json = case json of
  Aeson.Object m -> KeyMap.lookup (Key.fromText key) m
  Aeson.Array xs -> do
    i <- parseArrayIndex $ Text.unpack key
    xs Vector.!? i
  _ -> Nothing

-- | Parse an array index per RFC 6901 section 4.
-- Valid indices are "0" or a non-zero digit followed by any digits.
-- Leading zeros are not allowed (e.g., "01" is invalid).
-- The special "-" token is not supported for evaluation (it references
-- the nonexistent element after the last, used in JSON Patch).
parseArrayIndex :: String -> Maybe Int
parseArrayIndex "0" = Just 0
parseArrayIndex s@(c : cs)
  | c >= '1' && c <= '9' && all Char.isDigit cs = Just $ read s
parseArrayIndex _ = Nothing
