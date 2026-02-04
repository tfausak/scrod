{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Xml.Name where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

-- | XML Name (used for elements, attributes, and instructions)
-- Must be non-empty, starts with NameStartChar, then any number of NameChar
newtype Name = MkName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

-- | NameStartChar: letter, underscore, or colon (simplified)
isNameStartChar :: Char -> Bool
isNameStartChar c =
  Char.isAsciiUpper c
    || Char.isAsciiLower c
    || c == '_'
    || c == ':'

-- | NameChar: NameStartChar plus digits, hyphen, period
isNameChar :: Char -> Bool
isNameChar c =
  isNameStartChar c
    || Char.isDigit c
    || c == '-'
    || c == '.'

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Name
decode = do
  start <- Parsec.satisfy isNameStartChar
  rest <- Parsec.many $ Parsec.satisfy isNameChar
  pure . MkName . Text.pack $ start : rest

encode :: Name -> Builder.Builder
encode = Builder.stringUtf8 . Text.unpack . unwrap

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with a single letter" $ do
      Spec.assertEq s (Parsec.parseString decode "a") . Just . MkName $ Text.pack "a"

    Spec.it s "succeeds with multiple letters" $ do
      Spec.assertEq s (Parsec.parseString decode "foo") . Just . MkName $ Text.pack "foo"

    Spec.it s "succeeds with uppercase letters" $ do
      Spec.assertEq s (Parsec.parseString decode "FOO") . Just . MkName $ Text.pack "FOO"

    Spec.it s "succeeds with mixed case" $ do
      Spec.assertEq s (Parsec.parseString decode "FooBar") . Just . MkName $ Text.pack "FooBar"

    Spec.it s "succeeds with underscore start" $ do
      Spec.assertEq s (Parsec.parseString decode "_foo") . Just . MkName $ Text.pack "_foo"

    Spec.it s "succeeds with colon start" $ do
      Spec.assertEq s (Parsec.parseString decode ":foo") . Just . MkName $ Text.pack ":foo"

    Spec.it s "succeeds with digits after start" $ do
      Spec.assertEq s (Parsec.parseString decode "foo123") . Just . MkName $ Text.pack "foo123"

    Spec.it s "succeeds with hyphens" $ do
      Spec.assertEq s (Parsec.parseString decode "foo-bar") . Just . MkName $ Text.pack "foo-bar"

    Spec.it s "succeeds with periods" $ do
      Spec.assertEq s (Parsec.parseString decode "foo.bar") . Just . MkName $ Text.pack "foo.bar"

    Spec.it s "succeeds with namespace prefix" $ do
      Spec.assertEq s (Parsec.parseString decode "ns:element") . Just . MkName $ Text.pack "ns:element"

    Spec.it s "fails with digit start" $ do
      Spec.assertEq s (Parsec.parseString decode "1foo") Nothing

    Spec.it s "fails with hyphen start" $ do
      Spec.assertEq s (Parsec.parseString decode "-foo") Nothing

    Spec.it s "fails with period start" $ do
      Spec.assertEq s (Parsec.parseString decode ".foo") Nothing

    Spec.it s "fails with empty input" $ do
      Spec.assertEq s (Parsec.parseString decode "") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes a simple name" $ do
      Spec.assertEq s (Builder.toString . encode . MkName $ Text.pack "foo") "foo"

    Spec.it s "encodes a name with special chars" $ do
      Spec.assertEq s (Builder.toString . encode . MkName $ Text.pack "foo-bar_123") "foo-bar_123"
