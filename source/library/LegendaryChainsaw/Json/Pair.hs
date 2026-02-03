{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Json.Pair where

import qualified Data.ByteString.Builder as Builder
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Json.String as String
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

data Pair a = MkPair
  { name :: String.String
  , value :: a
  } deriving (Eq, Ord, Show)

decode :: Parsec.Stream s m Char => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m (Pair a)
decode p = do
  n <- String.decode <* Parsec.many Parsec.blank
  _ <- Parsec.char ':' <* Parsec.many Parsec.blank
  v <- p
  pure $ MkPair n v

encode :: (a -> Builder.Builder) -> Pair a -> Builder.Builder
encode b p = String.encode (name p) <> Builder.charUtf8 ':' <> b (value p)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  let pair :: String -> a -> Pair a
      pair = MkPair . String.MkString . Text.pack

  Spec.named s 'decode $ do
    let p :: Parsec.Stream t m Char => Parsec.ParsecT t u m String
        p = Parsec.many1 Parsec.digit

    Spec.it s "succeeds with simple pair" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "\"a\":1") . Just $ pair "a" "1"

    Spec.it s "succeeds with blank space after name" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "\"a\" :1") . Just $ pair "a" "1"

    Spec.it s "succeeds with blank space after separator" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "\"a\": 1") . Just $ pair "a" "1"

    Spec.it s "fails with missing name" $ do
      Spec.assertEq s (Parsec.parseString (decode p) ":1") Nothing

    Spec.it s "fails with missing separator" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "\"a\" 1") Nothing

    Spec.it s "fails with extra separator" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "\"a\"::1") Nothing

    Spec.it s "fails with missing value" $ do
      Spec.assertEq s (Parsec.parseString (decode p) ":1") Nothing

  Spec.named s 'encode $ do
    let b = Builder.integerDec

    Spec.it s "encodes simple pair" $ do
      Spec.assertEq s (Builder.toString . encode b $ pair "a" 1) "\"a\":1"

