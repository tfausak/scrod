-- | JSON Pointer implementation per RFC 6901.
-- https://datatracker.ietf.org/doc/html/rfc6901
module Scrod.Unstable.Type.Pointer
  ( -- * Types
    Pointer (..),
    Token (..),

    -- * Parsing
    parse,

    -- * Evaluation
    evaluate,
  )
where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json

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
  Text.replace (Text.pack "~0") (Text.pack "~")
    . Text.replace (Text.pack "~1") (Text.pack "/")

-- | Evaluate a JSON Pointer against a JSON value.
-- Returns Nothing if the pointer doesn't resolve to a value.
evaluate :: Pointer -> Json.Json -> Maybe Json.Json
evaluate (MkPointer []) json = Just json
evaluate (MkPointer (t : ts)) json = do
  next <- step t json
  evaluate (MkPointer ts) next

-- | Take a single step through a JSON value using a reference token.
step :: Token -> Json.Json -> Maybe Json.Json
step (MkToken key) json = case json of
  Json.Object m -> Map.lookup key m
  Json.Array xs -> do
    i <- parseArrayIndex $ Text.unpack key
    if i < length xs
      then Just $ xs List.!! i
      else Nothing
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
