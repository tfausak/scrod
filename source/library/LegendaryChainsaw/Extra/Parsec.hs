{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Extra.Parsec where

import qualified LegendaryChainsaw.Extra.Either as Either
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

blank :: Parsec.Stream s m Char => Parsec.ParsecT s u m Char
blank = Parsec.oneOf " \t\n\r"

parseString :: Parsec.Parsec String () a -> String -> Maybe a
parseString p = Either.hush . Parsec.parse p ""

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'blank $ do
    Spec.it s "succeeds with space" $ do
      Spec.assertEq s (parseString blank " ") $ Just ' '

    Spec.it s "succeeds with tab" $ do
      Spec.assertEq s (parseString blank "\t") $ Just '\t'

    Spec.it s "succeeds with line feed" $ do
      Spec.assertEq s (parseString blank "\n") $ Just '\n'

    Spec.it s "succeeds with carriage return" $ do
      Spec.assertEq s (parseString blank "\r") $ Just '\r'

    Spec.it s "fails with anything else" $ do
      Spec.assertEq s (parseString blank "x") Nothing

  Spec.named s 'parseString $ do
    Spec.it s "succeeds with valid input" $ do
      Spec.assertEq s (parseString Parsec.letter "a") $ Just 'a'

    Spec.it s "fails with invalid input" $ do
      Spec.assertEq s (parseString Parsec.letter "1") Nothing

    Spec.it s "allows unconsumed input" $ do
      Spec.assertEq s (parseString Parsec.letter "b1") $ Just 'b'
