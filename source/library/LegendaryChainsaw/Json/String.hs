{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Json.String where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Data.ByteString.Builder as Builder
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Ord as Ord
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Extra.Read as Read
import qualified LegendaryChainsaw.Extra.Semigroup as Semigroup
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

newtype String = MkString
  { unwrap :: Text.Text
  } deriving (Eq, Ord, Show)

decode :: Parsec.Stream s m Char => Parsec.ParsecT s u m LegendaryChainsaw.Json.String.String
decode = MkString . Text.pack <$> Parsec.between (Parsec.char '"') (Parsec.char '"') (Parsec.many decodeChar)

decodeChar :: Parsec.Stream s m Char => Parsec.ParsecT s u m Char
decodeChar = Parsec.choice
  [ decodeEscapedChar
  , decodeUnescapedChar
  ]

decodeEscapedChar :: Parsec.Stream s m Char => Parsec.ParsecT s u m Char
decodeEscapedChar = Parsec.choice
  [ decodeShortEscapedChar
  , decodeLongEscapedChar
  ]

decodeShortEscapedChar :: Parsec.Stream s m Char => Parsec.ParsecT s u m Char
decodeShortEscapedChar = Parsec.choice
  [ '"' <$ Parsec.string' "\\\""
  , '\\' <$ Parsec.string' "\\\\"
  , '/' <$ Parsec.string' "\\/"
  , '\b' <$ Parsec.string' "\\b"
  , '\f' <$ Parsec.string' "\\f"
  , '\n' <$ Parsec.string' "\\n"
  , '\r' <$ Parsec.string' "\\r"
  , '\t' <$ Parsec.string' "\\t"
  ]

decodeLongEscapedChar :: Parsec.Stream s m Char => Parsec.ParsecT s u m Char
decodeLongEscapedChar = do
  hi <- Parsec.string' "\\u" *> decodeWord16Hex
  if Ord.between 0xd800 0xdbff hi
    then do
      lo <- Parsec.string' "\\u" *> decodeWord16Hex
      Monad.unless (Ord.between 0xdc00 0xdfff lo) $ fail "invalid surrogate"
      pure . toEnum $ 0x10000 + ((fromIntegral hi - 0xd800) * 0x400) + (fromIntegral lo - 0xdc00)
    else pure . toEnum $ fromIntegral hi

decodeWord16Hex :: Parsec.Stream s m Char => Parsec.ParsecT s u m Word.Word16
decodeWord16Hex = do
  ds <- Parsec.count 4 Parsec.hexDigit
  maybe (fail "invalid escape") pure . Read.readM $ "0x" <> ds

decodeUnescapedChar :: Parsec.Stream s m Char => Parsec.ParsecT s u m Char
decodeUnescapedChar = Parsec.satisfy $ \ c -> c >= ' ' && c /= '"' && c /= '\\'

encode :: LegendaryChainsaw.Json.String.String -> Builder.Builder
encode = Semigroup.around (Builder.char8 '"') (Builder.char8 '"') . foldMap encodeChar . Text.unpack . unwrap

encodeChar :: Char -> Builder.Builder
encodeChar c = case c of
  '"'  -> Builder.stringUtf8 "\\\""
  '\\' -> Builder.stringUtf8 "\\\\"
  '\b' -> Builder.stringUtf8 "\\b"
  '\f' -> Builder.stringUtf8 "\\f"
  '\n' -> Builder.stringUtf8 "\\n"
  '\r' -> Builder.stringUtf8 "\\r"
  '\t' -> Builder.stringUtf8 "\\t"
  _ -> if c >= ' '
    then Builder.charUtf8 c
    else Builder.stringUtf8 "\\u" <> Builder.word16HexFixed (fromIntegral $ fromEnum c)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with an empty string" $ do
      Spec.assertEq s (Parsec.parseString decode "\"\"") . Just . MkString $ Text.pack ""

    Spec.it s "succeeds with a single character" $ do
      Spec.assertEq s (Parsec.parseString decode "\"a\"") . Just . MkString $ Text.pack "a"

    Spec.it s "succeeds with two characters" $ do
      Spec.assertEq s (Parsec.parseString decode "\"ab\"") . Just . MkString $ Text.pack "ab"

    Spec.it s "succeeds with an escaped quotation mark" $ do
      Spec.assertEq s (Parsec.parseString decode "\" \\\" \"") . Just . MkString $ Text.pack " \" "

    Spec.it s "succeeds with an escaped reverse solidus" $ do
      Spec.assertEq s (Parsec.parseString decode "\" \\\\ \"") . Just . MkString $ Text.pack " \\ "

    Spec.it s "succeeds with an escaped solidus" $ do
      Spec.assertEq s (Parsec.parseString decode "\" \\/ \"") . Just . MkString $ Text.pack " / "

    Spec.it s "succeeds with an escaped backspace" $ do
      Spec.assertEq s (Parsec.parseString decode "\" \\b \"") . Just . MkString $ Text.pack " \b "

    Spec.it s "succeeds with an escaped form feed" $ do
      Spec.assertEq s (Parsec.parseString decode "\" \\f \"") . Just . MkString $ Text.pack " \f "

    Spec.it s "succeeds with an escaped line feed" $ do
      Spec.assertEq s (Parsec.parseString decode "\" \\n \"") . Just . MkString $ Text.pack " \n "

    Spec.it s "succeeds with an escaped carriage return" $ do
      Spec.assertEq s (Parsec.parseString decode "\" \\r \"") . Just . MkString $ Text.pack " \r "

    Spec.it s "succeeds with an escaped tab" $ do
      Spec.assertEq s (Parsec.parseString decode "\" \\t \"") . Just . MkString $ Text.pack " \t "

    Spec.it s "fails with an unescaped tab" $ do
      Spec.assertEq s (Parsec.parseString decode "\" \t \"") Nothing

    Spec.it s "succeeds with an escaped control character" $ do
      Spec.assertEq s (Parsec.parseString decode "\" \\u001f \"") . Just . MkString $ Text.pack " \x1f "

    Spec.it s "succeeds with an unnecessarily escaped character" $ do
      Spec.assertEq s (Parsec.parseString decode "\" \\u006F \"") . Just . MkString $ Text.pack " o "

    Spec.it s "succeeds with a surrogate pair" $ do
      Spec.assertEq s (Parsec.parseString decode "\" \\uD834\\uDD1E \"") . Just . MkString $ Text.pack " \x1d11e "

    Spec.it s "fails with an unpaired surrogate" $ do
      Spec.assertEq s (Parsec.parseString decode "\"\\uD800\"") Nothing

    Spec.it s "fails with an invalid low surrogate" $ do
      Spec.assertEq s (Parsec.parseString decode "\"\\uD800\\u0000\"") Nothing

  Spec.named s 'encode $ do
    Spec.it s "works with an empty string" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack "") "\"\""

    Spec.it s "works with one character" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack "a") "\"a\""

    Spec.it s "works with two characters" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack "ab") "\"ab\""

    Spec.it s "escapes a quotation mark" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack " \" ") "\" \\\" \""

    Spec.it s "escapes a reverse solidus" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack " \\ ") "\" \\\\ \""

    Spec.it s "does not escape a solidus" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack " / ") "\" / \""

    Spec.it s "escapes a backspace" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack " \b ") "\" \\b \""

    Spec.it s "escapes a form feed" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack " \f ") "\" \\f \""

    Spec.it s "escapes a line feed" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack " \n ") "\" \\n \""

    Spec.it s "escapes a carriage return" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack " \r ") "\" \\r \""

    Spec.it s "escapes a tab" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack " \t ") "\" \\t \""

    Spec.it s "escapes a control character" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack " \x1f ") "\" \\u001f \""

    Spec.it s "does not escape a two-byte character" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack " \x80 ") "\" \xc2\x80 \""

    Spec.it s "does not escape a three-byte character" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack " \x800 ") "\" \xe0\xa0\x80 \""

    Spec.it s "does not escape a four-byte character" $ do
      Spec.assertEq s (Builder.toString . encode . MkString $ Text.pack " \x10000 ") "\" \xf0\x90\x80\x80 \""
