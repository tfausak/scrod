{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Scrod.Unstable.Type.Json
  ( -- * Types
    Json,
    pattern Null,
    pattern Boolean,
    pattern Number,
    pattern String,
    pattern Array,
    pattern Object,

    -- * Parsing
    parse,

    -- * Rendering
    render,

    -- * Construction helpers
    tag,
    tagged,
    object,
    fromNatural,
    fromInt,
    integerToJson,
    fromText,
    fromBool,
    fromList,
    fromMap,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector
import qualified Numeric.Natural as Natural

-- * Types

-- | A JSON value. Type alias for Aeson's Value type.
type Json = Aeson.Value

-- | Pattern synonym for JSON null.
pattern Null :: Json
pattern Null = Aeson.Null

-- | Pattern synonym for JSON booleans.
pattern Boolean :: Bool -> Json
pattern Boolean b = Aeson.Bool b

-- | Pattern synonym for JSON numbers.
pattern Number :: Scientific.Scientific -> Json
pattern Number n = Aeson.Number n

-- | Pattern synonym for JSON strings.
pattern String :: Text.Text -> Json
pattern String t = Aeson.String t

-- | Pattern synonym for JSON arrays.
pattern Array :: Vector.Vector Json -> Json
pattern Array xs = Aeson.Array xs

-- | Pattern synonym for JSON objects.
pattern Object :: KeyMap.KeyMap Json -> Json
pattern Object m = Aeson.Object m

{-# COMPLETE Null, Boolean, Number, String, Array, Object #-}

-- * Parsing

-- | Parse JSON text into a Json value.
parse :: Text.Text -> Either String Json
parse = Aeson.eitherDecodeStrict' . Encoding.encodeUtf8

-- * Rendering

-- | Render a Json value to a lazy ByteString.
render :: Json -> LazyByteString.ByteString
render = Aeson.encode

-- * Construction helpers

-- | Create a tagged object for sum types without contents.
-- Example: @tag "Value"@ produces @{"tag": "Value"}@
tag :: Text.Text -> Json
tag t = Object $ KeyMap.singleton "tag" (String t)

-- | Create a tagged object with contents.
-- Example: @tagged "Some" (Number 42)@ produces @{"tag": "Some", "contents": 42}@
tagged :: Text.Text -> Json -> Json
tagged t contents =
  Object $
    KeyMap.fromList
      [ ("tag", String t),
        ("contents", contents)
      ]

-- | Create a JSON object from a list of key-value pairs.
object :: [(Text.Text, Json)] -> Json
object = fromMap . Map.fromList

-- | Convert a Natural to a JSON Number.
fromNatural :: Natural.Natural -> Json
fromNatural n = Number $ Scientific.scientific (toInteger n) 0

-- | Convert an Int to a JSON Number.
fromInt :: Int -> Json
fromInt = integerToJson . toInteger

-- | Convert an Integer to a JSON Number.
integerToJson :: Integer -> Json
integerToJson n = Number $ Scientific.scientific n 0

-- | Convert Text to a JSON String.
fromText :: Text.Text -> Json
fromText = String

-- | Convert Bool to a JSON Boolean.
fromBool :: Bool -> Json
fromBool = Boolean

-- | Convert a list to a JSON Array.
fromList :: [Json] -> Json
fromList = Array . Vector.fromList

-- | Convert a Map to a JSON Object.
fromMap :: Map.Map Text.Text Json -> Json
fromMap = Object . KeyMap.fromList . fmap (Bifunctor.first Key.fromText) . Map.toList
