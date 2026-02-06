{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.JsonPointer.Token where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Spec as Spec
import qualified Text.Parsec as Parsec

-- | A reference token in a JSON Pointer, as defined by RFC 6901.
-- The token stores the unescaped text value.
newtype Token = MkToken
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

-- | Decodes a reference token from a JSON Pointer string.
-- Handles the escape sequences: ~0 -> ~, ~1 -> /
-- Per RFC 6901, we must decode ~1 before ~0 to avoid errors.
decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Token
decode = MkToken . Text.pack <$> Parsec.many decodeChar

decodeChar :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Char
decodeChar =
  Parsec.choice
    [ '/' <$ Parsec.string' "~1",
      '~' <$ Parsec.string' "~0",
      Parsec.noneOf "/"
    ]

-- | Encodes a token for use in a JSON Pointer string.
-- Escapes ~ as ~0 and / as ~1.
-- Per RFC 6901, we must encode ~ before / to maintain round-trip consistency.
encode :: Token -> Builder.Builder
encode = foldMap encodeChar . Text.unpack . unwrap

encodeChar :: Char -> Builder.Builder
encodeChar c = case c of
  '~' -> Builder.stringUtf8 "~0"
  '/' -> Builder.stringUtf8 "~1"
  _ -> Builder.charUtf8 c

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  let token = MkToken . Text.pack

  Spec.named s 'decode $ do
    Spec.it s "succeeds with an empty token" $ do
      Spec.assertEq s (Parsec.parseString decode "") . Just $ token ""

    Spec.it s "succeeds with a simple token" $ do
      Spec.assertEq s (Parsec.parseString decode "foo") . Just $ token "foo"

    Spec.it s "succeeds with a numeric token" $ do
      Spec.assertEq s (Parsec.parseString decode "0") . Just $ token "0"

    Spec.it s "decodes ~0 as tilde" $ do
      Spec.assertEq s (Parsec.parseString decode "~0") . Just $ token "~"

    Spec.it s "decodes ~1 as slash" $ do
      Spec.assertEq s (Parsec.parseString decode "~1") . Just $ token "/"

    Spec.it s "decodes ~01 as ~1 (not as /)" $ do
      Spec.assertEq s (Parsec.parseString decode "~01") . Just $ token "~1"

    Spec.it s "decodes a~1b as a/b" $ do
      Spec.assertEq s (Parsec.parseString decode "a~1b") . Just $ token "a/b"

    Spec.it s "decodes m~0n as m~n" $ do
      Spec.assertEq s (Parsec.parseString decode "m~0n") . Just $ token "m~n"

    Spec.it s "stops at slash" $ do
      Spec.assertEq s (Parsec.parseString (decode <* Parsec.char '/') "foo/") . Just $ token "foo"

    Spec.it s "handles multiple escapes" $ do
      Spec.assertEq s (Parsec.parseString decode "~0~1~0~1") . Just $ token "~/~/"

  Spec.named s 'encode $ do
    Spec.it s "works with an empty token" $ do
      Spec.assertEq s (Builder.toString . encode $ token "") ""

    Spec.it s "works with a simple token" $ do
      Spec.assertEq s (Builder.toString . encode $ token "foo") "foo"

    Spec.it s "works with a numeric token" $ do
      Spec.assertEq s (Builder.toString . encode $ token "0") "0"

    Spec.it s "encodes tilde as ~0" $ do
      Spec.assertEq s (Builder.toString . encode $ token "~") "~0"

    Spec.it s "encodes slash as ~1" $ do
      Spec.assertEq s (Builder.toString . encode $ token "/") "~1"

    Spec.it s "encodes a/b as a~1b" $ do
      Spec.assertEq s (Builder.toString . encode $ token "a/b") "a~1b"

    Spec.it s "encodes m~n as m~0n" $ do
      Spec.assertEq s (Builder.toString . encode $ token "m~n") "m~0n"

    Spec.it s "handles multiple special chars" $ do
      Spec.assertEq s (Builder.toString . encode $ token "~/~/") "~0~1~0~1"
