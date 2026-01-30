{-# LANGUAGE PatternSynonyms #-}

module Scrod.Type.Json
  ( -- * Types
    Aeson.Value,
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

-- | Pattern synonym for JSON null.
pattern Null :: Aeson.Value
pattern Null = Aeson.Null

-- | Pattern synonym for JSON booleans.
pattern Boolean :: Bool -> Aeson.Value
pattern Boolean b = Aeson.Bool b

-- | Pattern synonym for JSON numbers.
pattern Number :: Scientific.Scientific -> Aeson.Value
pattern Number n = Aeson.Number n

-- | Pattern synonym for JSON strings.
pattern String :: Text.Text -> Aeson.Value
pattern String t = Aeson.String t

-- | Pattern synonym for JSON arrays.
pattern Array :: Vector.Vector Aeson.Value -> Aeson.Value
pattern Array xs = Aeson.Array xs

-- | Pattern synonym for JSON objects.
pattern Object :: KeyMap.KeyMap Aeson.Value -> Aeson.Value
pattern Object m = Aeson.Object m

{-# COMPLETE Null, Boolean, Number, String, Array, Object #-}

-- * Parsing

-- | Parse JSON text into a Value.
parse :: Text.Text -> Either String Aeson.Value
parse = Aeson.eitherDecodeStrict' . Encoding.encodeUtf8

-- * Rendering

-- | Render a Value to a lazy ByteString.
render :: Aeson.Value -> LazyByteString.ByteString
render = Aeson.encode
