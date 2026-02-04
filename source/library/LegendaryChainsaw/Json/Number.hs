{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Json.Number where

import qualified Data.ByteString.Builder as Builder
import qualified LegendaryChainsaw.Decimal as Decimal
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Extra.Read as Read
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

newtype Number = MkNumber
  { unwrap :: Decimal.Decimal
  }
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Number
decode = do
  sign <- decodeSign
  intPart <- decodeInt
  (fracPart, fracExp) <- decodeFrac
  expPart <- decodeExp
  pure . MkNumber $
    Decimal.mkDecimal
      (sign $ intPart * (10 ^ abs fracExp) + fracPart)
      (fracExp + expPart)

decodeSign :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m (Integer -> Integer)
decodeSign = Parsec.option id (negate <$ Parsec.char '-')

decodeInt :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Integer
decodeInt =
  Parsec.choice
    [ 0 <$ Parsec.char '0' <* Parsec.notFollowedBy Parsec.digit,
      do
        first <- Parsec.satisfy $ \c -> c >= '1' && c <= '9'
        rest <- Parsec.many Parsec.digit
        maybe (fail "invalid integer") pure . Read.readM $ first : rest
    ]

decodeFrac :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m (Integer, Integer)
decodeFrac = Parsec.option (0, 0) $ do
  digits <- Parsec.char '.' *> Parsec.many1 Parsec.digit
  fracValue <- maybe (fail "invalid fraction") pure $ Read.readM digits
  pure (fracValue, negate . toInteger $ length digits)

decodeExp :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Integer
decodeExp = Parsec.option 0 $ do
  _ <- Parsec.oneOf "eE"
  expSign <-
    Parsec.option id $
      Parsec.choice
        [ id <$ Parsec.char '+',
          negate <$ Parsec.char '-'
        ]
  expDigits <- Parsec.many1 Parsec.digit
  maybe (fail "invalid exponent") (pure . expSign) $ Read.readM expDigits

encode :: Number -> Builder.Builder
encode n =
  let d = unwrap n
   in Builder.integerDec (Decimal.mantissa d)
        <> Builder.charUtf8 'e'
        <> Builder.integerDec (Decimal.exponent d)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "parses zero" $ do
      Spec.assertEq s (Parsec.parseString decode "0") . Just . MkNumber $ Decimal.mkDecimal 0 0

    Spec.it s "parses positive integer" $ do
      Spec.assertEq s (Parsec.parseString decode "123") . Just . MkNumber $ Decimal.mkDecimal 123 0

    Spec.it s "parses negative integer" $ do
      Spec.assertEq s (Parsec.parseString decode "-123") . Just . MkNumber $ Decimal.mkDecimal (-123) 0

    Spec.it s "parses decimal with fraction" $ do
      Spec.assertEq s (Parsec.parseString decode "123.45") . Just . MkNumber $ Decimal.mkDecimal 12345 (-2)

    Spec.it s "parses negative decimal with fraction" $ do
      Spec.assertEq s (Parsec.parseString decode "-123.45") . Just . MkNumber $ Decimal.mkDecimal (-12345) (-2)

    Spec.it s "parses with positive exponent" $ do
      Spec.assertEq s (Parsec.parseString decode "123e2") . Just . MkNumber $ Decimal.mkDecimal 123 2

    Spec.it s "parses with negative exponent" $ do
      Spec.assertEq s (Parsec.parseString decode "123e-2") . Just . MkNumber $ Decimal.mkDecimal 123 (-2)

    Spec.it s "parses with uppercase E" $ do
      Spec.assertEq s (Parsec.parseString decode "123E2") . Just . MkNumber $ Decimal.mkDecimal 123 2

    Spec.it s "parses with explicit plus in exponent" $ do
      Spec.assertEq s (Parsec.parseString decode "123e+2") . Just . MkNumber $ Decimal.mkDecimal 123 2

    Spec.it s "parses fraction with exponent" $ do
      Spec.assertEq s (Parsec.parseString decode "1.23e5") . Just . MkNumber $ Decimal.mkDecimal 123 3

    Spec.it s "fails with leading zero" $ do
      Spec.assertEq s (Parsec.parseString decode "01") Nothing

    Spec.it s "fails with just minus" $ do
      Spec.assertEq s (Parsec.parseString decode "-") Nothing

    Spec.it s "fails with trailing fraction" $ do
      Spec.assertEq s (Parsec.parseString decode "123.") Nothing

    Spec.it s "fails with leading fraction" $ do
      Spec.assertEq s (Parsec.parseString decode ".123") Nothing

    Spec.it s "fails with trailing exponent" $ do
      Spec.assertEq s (Parsec.parseString decode "123e") Nothing

    Spec.it s "fails with leading exponent" $ do
      Spec.assertEq s (Parsec.parseString decode "e123") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes zero" $ do
      Spec.assertEq s (Builder.toString . encode . MkNumber $ Decimal.mkDecimal 0 0) "0e0"

    Spec.it s "encodes positive integer" $ do
      Spec.assertEq s (Builder.toString . encode . MkNumber $ Decimal.mkDecimal 123 0) "123e0"

    Spec.it s "encodes negative integer" $ do
      Spec.assertEq s (Builder.toString . encode . MkNumber $ Decimal.mkDecimal (-123) 0) "-123e0"

    Spec.it s "encodes with positive exponent" $ do
      Spec.assertEq s (Builder.toString . encode . MkNumber $ Decimal.mkDecimal 123 2) "123e2"

    Spec.it s "encodes with negative exponent" $ do
      Spec.assertEq s (Builder.toString . encode . MkNumber $ Decimal.mkDecimal 12345 (-2)) "12345e-2"

    Spec.it s "encodes small fraction" $ do
      Spec.assertEq s (Builder.toString . encode . MkNumber $ Decimal.mkDecimal 123 (-5)) "123e-5"
