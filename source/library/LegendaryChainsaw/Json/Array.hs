{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Json.Array where

import qualified Data.ByteString.Builder as Builder
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Monoid as Monoid
import qualified LegendaryChainsaw.Extra.Semigroup as Semigroup
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

newtype Array a = MkArray
  { unwrap :: [a]
  } deriving (Eq, Ord, Show)

decode :: Parsec.Stream s m Char => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m (Array a)
decode p = fmap MkArray
  . Parsec.between (Parsec.char '[' <* Parsec.many Parsec.blank) (Parsec.char ']')
  $ Parsec.sepBy (p <* Parsec.many Parsec.blank) (Parsec.char ',' <* Parsec.many Parsec.blank)

encode :: (a -> Builder.Builder) -> Array a -> Builder.Builder
encode b = Semigroup.around (Builder.charUtf8 '[') (Builder.charUtf8 ']')
  . Monoid.sepBy (Builder.charUtf8 ',')
  . fmap b
  . unwrap

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    let p :: Parsec.Stream t m Char => Parsec.ParsecT t u m String
        p = Parsec.many1 Parsec.digit

    Spec.it s "succeeds with an empty array" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "[]") . Just $ MkArray []

    Spec.it s "succeeds with an empty array with blank space" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "[ ]") . Just $ MkArray []

    Spec.it s "succeeds with a single element" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "[1]") . Just $ MkArray ["1"]

    Spec.it s "succeeds with a single element with blank space" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "[ 1 ]") . Just $ MkArray ["1"]

    Spec.it s "succeeds with multiple elements" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "[1,2]") . Just $ MkArray ["1", "2"]

    Spec.it s "succeeds with multiple elements with blank space" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "[ 1 , 2 ]") . Just $ MkArray ["1", "2"]

    Spec.it s "fails with leading comma" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "[,1]") Nothing

    Spec.it s "fails with trailing comma" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "[1,]") Nothing

    Spec.it s "fails with extra comma" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "[1,,2]") Nothing

    Spec.it s "fails with only comma" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "[,]") Nothing

  Spec.named s 'encode $ do
    let b = Builder.integerDec

    Spec.it s "encodes empty array" $ do
      Spec.assertEq s (Builder.toString . encode b $ MkArray []) "[]"

    Spec.it s "encodes single element" $ do
      Spec.assertEq s (Builder.toString . encode b $ MkArray [1]) "[1]"

    Spec.it s "encodes multiple elements" $ do
      Spec.assertEq s (Builder.toString . encode b $ MkArray [1, 2]) "[1,2]"
