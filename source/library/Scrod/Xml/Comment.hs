{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Comment where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Extra.Semigroup as Semigroup
import qualified Scrod.Spec as Spec
import qualified Text.Parsec as Parsec

-- | XML Comment like @\<!-- comment text -->@. Cannot contain @-->@.
newtype Comment = MkComment
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Comment
decode = MkComment . Text.pack <$> Parsec.between (Parsec.string' "<!--") (Parsec.string' "-->") decodeContent

decodeContent :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m String
decodeContent = Parsec.manyTill Parsec.anyChar . Parsec.lookAhead $ Parsec.string' "-->"

encode :: Comment -> Builder.Builder
encode =
  Semigroup.around (Builder.stringUtf8 "<!--") (Builder.stringUtf8 "-->")
    . Builder.stringUtf8
    . Text.unpack
    . unwrap

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with empty comment" $ do
      Spec.assertEq s (Parsec.parseString decode "<!---->") . Just . MkComment $ Text.pack ""

    Spec.it s "succeeds with single space" $ do
      Spec.assertEq s (Parsec.parseString decode "<!-- -->") . Just . MkComment $ Text.pack " "

    Spec.it s "succeeds with text" $ do
      Spec.assertEq s (Parsec.parseString decode "<!-- hello -->") . Just . MkComment $ Text.pack " hello "

    Spec.it s "succeeds with special chars" $ do
      Spec.assertEq s (Parsec.parseString decode "<!-- & < > -->") . Just . MkComment $ Text.pack " & < > "

    Spec.it s "succeeds with single dash" $ do
      Spec.assertEq s (Parsec.parseString decode "<!-- - -->") . Just . MkComment $ Text.pack " - "

    Spec.it s "succeeds with double dash not followed by >" $ do
      Spec.assertEq s (Parsec.parseString decode "<!-- -- x -->") . Just . MkComment $ Text.pack " -- x "

    Spec.it s "fails without opening" $ do
      Spec.assertEq s (Parsec.parseString decode "hello -->") Nothing

    Spec.it s "fails without closing" $ do
      Spec.assertEq s (Parsec.parseString decode "<!-- hello") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes empty comment" $ do
      Spec.assertEq s (Builder.toString . encode . MkComment $ Text.pack "") "<!---->"

    Spec.it s "encodes comment with text" $ do
      Spec.assertEq s (Builder.toString . encode . MkComment $ Text.pack " hello ") "<!-- hello -->"

    Spec.it s "encodes comment with special chars" $ do
      Spec.assertEq s (Builder.toString . encode . MkComment $ Text.pack " & ") "<!-- & -->"
