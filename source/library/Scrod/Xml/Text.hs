{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Text where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Builder as Builder
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Extra.Read as Read
import qualified Scrod.Spec as Spec
import qualified Text.Parsec as Parsec

-- | XML text content with entity support
-- Entities: &amp; &lt; &gt; &apos; &quot; &#123; &#x1a;
decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Text.Text
decode = Text.pack <$> Parsec.many decodeChar

decodeChar :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Char
decodeChar =
  Parsec.choice
    [ decodeEntity,
      decodeUnescapedChar
    ]

decodeEntity :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Char
decodeEntity =
  Parsec.char '&'
    *> Parsec.choice
      [ '&' <$ Parsec.string' "amp;",
        '<' <$ Parsec.string' "lt;",
        '>' <$ Parsec.string' "gt;",
        '\'' <$ Parsec.string' "apos;",
        '"' <$ Parsec.string' "quot;",
        decodeNumericEntity
      ]

decodeNumericEntity :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Char
decodeNumericEntity =
  Parsec.char '#'
    *> Parsec.choice
      [ decodeHexEntity,
        decodeDecEntity
      ]

decodeHexEntity :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Char
decodeHexEntity = do
  _ <- Parsec.oneOf "xX"
  ds <- Parsec.many1 Parsec.hexDigit
  _ <- Parsec.char ';'
  n <- maybe (fail "invalid hex entity") pure . Read.readM $ "0x" <> ds
  Monad.when (n < 0 || n > 0x10ffff) $ fail "code point out of range"
  pure $ Char.chr n

decodeDecEntity :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Char
decodeDecEntity = do
  ds <- Parsec.many1 Parsec.digit
  _ <- Parsec.char ';'
  n <- maybe (fail "invalid decimal entity") pure $ Read.readM ds
  Monad.when (n < 0 || n > 0x10ffff) $ fail "code point out of range"
  pure $ Char.chr n

decodeUnescapedChar :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Char
decodeUnescapedChar = Parsec.satisfy $ \c -> c /= '<' && c /= '&'

encode :: Text.Text -> Builder.Builder
encode = foldMap encodeChar . Text.unpack

encodeChar :: Char -> Builder.Builder
encodeChar c = case c of
  '&' -> Builder.stringUtf8 "&amp;"
  '<' -> Builder.stringUtf8 "&lt;"
  '>' -> Builder.stringUtf8 "&gt;"
  _ -> Builder.charUtf8 c

-- | Encode for attribute values (also escapes quotes)
encodeAttr :: Text.Text -> Builder.Builder
encodeAttr = foldMap encodeAttrChar . Text.unpack

encodeAttrChar :: Char -> Builder.Builder
encodeAttrChar c = case c of
  '\'' -> Builder.stringUtf8 "&apos;"
  '"' -> Builder.stringUtf8 "&quot;"
  _ -> encodeChar c

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with empty text" $ do
      Spec.assertEq s (Parsec.parseString decode "") . Just $ Text.pack ""

    Spec.it s "succeeds with plain text" $ do
      Spec.assertEq s (Parsec.parseString decode "hello world") . Just $ Text.pack "hello world"

    Spec.it s "succeeds with amp entity" $ do
      Spec.assertEq s (Parsec.parseString decode "a &amp; b") . Just $ Text.pack "a & b"

    Spec.it s "succeeds with lt entity" $ do
      Spec.assertEq s (Parsec.parseString decode "a &lt; b") . Just $ Text.pack "a < b"

    Spec.it s "succeeds with gt entity" $ do
      Spec.assertEq s (Parsec.parseString decode "a &gt; b") . Just $ Text.pack "a > b"

    Spec.it s "succeeds with apos entity" $ do
      Spec.assertEq s (Parsec.parseString decode "a &apos; b") . Just $ Text.pack "a ' b"

    Spec.it s "succeeds with quot entity" $ do
      Spec.assertEq s (Parsec.parseString decode "a &quot; b") . Just $ Text.pack "a \" b"

    Spec.it s "succeeds with decimal entity" $ do
      Spec.assertEq s (Parsec.parseString decode "&#65;") . Just $ Text.pack "A"

    Spec.it s "succeeds with hex entity lowercase" $ do
      Spec.assertEq s (Parsec.parseString decode "&#x41;") . Just $ Text.pack "A"

    Spec.it s "succeeds with hex entity uppercase" $ do
      Spec.assertEq s (Parsec.parseString decode "&#X41;") . Just $ Text.pack "A"

    Spec.it s "succeeds with multiple entities" $ do
      Spec.assertEq s (Parsec.parseString decode "&lt;tag&gt;") . Just $ Text.pack "<tag>"

    Spec.it s "stops at less-than" $ do
      Spec.assertEq s (Parsec.parseString decode "hello<world") . Just $ Text.pack "hello"

  Spec.named s 'encode $ do
    Spec.it s "encodes empty text" $ do
      Spec.assertEq s (Builder.toString . encode $ Text.pack "") ""

    Spec.it s "encodes plain text" $ do
      Spec.assertEq s (Builder.toString . encode $ Text.pack "hello") "hello"

    Spec.it s "encodes ampersand" $ do
      Spec.assertEq s (Builder.toString . encode $ Text.pack "a & b") "a &amp; b"

    Spec.it s "encodes less-than" $ do
      Spec.assertEq s (Builder.toString . encode $ Text.pack "a < b") "a &lt; b"

    Spec.it s "encodes greater-than" $ do
      Spec.assertEq s (Builder.toString . encode $ Text.pack "a > b") "a &gt; b"

  Spec.named s 'encodeAttr $ do
    Spec.it s "encodes single quote" $ do
      Spec.assertEq s (Builder.toString . encodeAttr $ Text.pack "a ' b") "a &apos; b"

    Spec.it s "encodes double quote" $ do
      Spec.assertEq s (Builder.toString . encodeAttr $ Text.pack "a \" b") "a &quot; b"
