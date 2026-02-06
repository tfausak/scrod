{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Json.Object where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Monoid as Monoid
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Extra.Semigroup as Semigroup
import qualified Scrod.Json.Pair as Pair
import qualified Scrod.Json.String as String
import qualified Scrod.Spec as Spec
import qualified Text.Parsec as Parsec

newtype Object a = MkObject
  { unwrap :: [Pair.Pair a]
  }
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m (Object a)
decode p =
  fmap MkObject
    . Parsec.between (Parsec.char '{' <* Parsec.many Parsec.blank) (Parsec.char '}')
    $ Parsec.sepBy (Pair.decode p <* Parsec.many Parsec.blank) (Parsec.char ',' <* Parsec.many Parsec.blank)

encode :: (a -> Builder.Builder) -> Object a -> Builder.Builder
encode b =
  Semigroup.around (Builder.charUtf8 '{') (Builder.charUtf8 '}')
    . Monoid.sepBy (Builder.charUtf8 ',')
    . fmap (Pair.encode b)
    . unwrap

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  let object :: [(String, a)] -> Object a
      object = MkObject . fmap (\(n, v) -> Pair.MkPair (String.MkString $ Text.pack n) v)

  Spec.named s 'decode $ do
    let p :: (Parsec.Stream t m Char) => Parsec.ParsecT t u m Prelude.String
        p = Parsec.many1 Parsec.digit

    Spec.it s "succeeds with an empty object" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "{}") . Just $ object []

    Spec.it s "succeeds with an empty object with blank space" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "{ }") . Just $ object []

    Spec.it s "succeeds with a single pair" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "{\"a\":1}") . Just $ object [("a", "1")]

    Spec.it s "succeeds with a single pair with blank space" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "{ \"a\":1 }") . Just $ object [("a", "1")]

    Spec.it s "succeeds with multiple pairs" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "{\"a\":1,\"b\":2}") . Just $ object [("a", "1"), ("b", "2")]

    Spec.it s "succeeds with multiple pairs with blank space" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "{ \"a\":1 , \"b\":2 }") . Just $ object [("a", "1"), ("b", "2")]

    Spec.it s "fails with leading comma" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "{,\"a\":1}") Nothing

    Spec.it s "fails with trailing comma" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "{\"a\":1,}") Nothing

    Spec.it s "fails with extra comma" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "{\"a\":1,,\"b\":2}") Nothing

    Spec.it s "fails with only comma" $ do
      Spec.assertEq s (Parsec.parseString (decode p) "{,}") Nothing

  Spec.named s 'encode $ do
    let b = Builder.integerDec

    Spec.it s "encodes empty object" $ do
      Spec.assertEq s (Builder.toString . encode b $ MkObject []) "{}"

    Spec.it s "encodes single pair" $ do
      Spec.assertEq s (Builder.toString . encode b $ object [("a", 1)]) "{\"a\":1}"

    Spec.it s "encodes multiple pairs" $ do
      Spec.assertEq s (Builder.toString . encode b $ object [("a", 1), ("b", 2)]) "{\"a\":1,\"b\":2}"
