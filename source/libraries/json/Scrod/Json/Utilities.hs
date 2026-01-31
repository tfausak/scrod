module Scrod.Json.Utilities
  ( -- * Types
    Aeson.Value,

    -- * Parsing
    parse,

    -- * Rendering
    render,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

-- * Parsing

-- | Parse JSON text into a Value.
parse :: Text.Text -> Either String Aeson.Value
parse = Aeson.eitherDecodeStrict' . Encoding.encodeUtf8

-- * Rendering

-- | Render a Value to a lazy ByteString.
render :: Aeson.Value -> LazyByteString.ByteString
render = Aeson.encode
