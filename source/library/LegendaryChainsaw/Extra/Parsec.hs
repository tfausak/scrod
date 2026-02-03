{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Extra.Parsec where

import qualified Data.Functor.Identity as Identity
import qualified LegendaryChainsaw.Extra.Either as Either
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

parseString :: Parsec.ParsecT String () Identity.Identity a -> String -> Maybe a
parseString p = Either.hush . Parsec.parse p ""

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'parseString $ do
    Spec.it s "succeeds with valid input" $ do
      Spec.assertEq s (parseString Parsec.letter "a") $ Just 'a'

    Spec.it s "fails with invalid input" $ do
      Spec.assertEq s (parseString Parsec.letter "1") Nothing

    Spec.it s "allows unconsumed input" $ do
      Spec.assertEq s (parseString Parsec.letter "b1") $ Just 'b'
