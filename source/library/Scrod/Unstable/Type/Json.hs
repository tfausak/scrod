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
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector

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
