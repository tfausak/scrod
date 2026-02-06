{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Css.Name where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Spec as Spec
import qualified Text.Parsec as Parsec

-- | CSS Name (used for property names and at-rule names)
-- Must be non-empty, starts with letter, hyphen, or underscore
-- Continues with letters, digits, hyphens, or underscores
newtype Name = MkName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

-- | NameStartChar: letter, hyphen, or underscore
isNameStartChar :: Char -> Bool
isNameStartChar c =
  Char.isAsciiUpper c
    || Char.isAsciiLower c
    || c == '-'
    || c == '_'

-- | NameChar: letter, digit, hyphen, or underscore
isNameChar :: Char -> Bool
isNameChar c =
  isNameStartChar c
    || Char.isDigit c

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

    Spec.it s "succeeds with hyphen start" $ do
      Spec.assertEq s (Parsec.parseString decode "-webkit-transform") . Just . MkName $ Text.pack "-webkit-transform"

    Spec.it s "succeeds with underscore start" $ do
      Spec.assertEq s (Parsec.parseString decode "_custom") . Just . MkName $ Text.pack "_custom"

    Spec.it s "succeeds with digits after start" $ do
      Spec.assertEq s (Parsec.parseString decode "h1") . Just . MkName $ Text.pack "h1"

    Spec.it s "succeeds with hyphens" $ do
      Spec.assertEq s (Parsec.parseString decode "background-color") . Just . MkName $ Text.pack "background-color"

    Spec.it s "succeeds with underscores" $ do
      Spec.assertEq s (Parsec.parseString decode "my_property") . Just . MkName $ Text.pack "my_property"

    Spec.it s "fails with digit start" $ do
      Spec.assertEq s (Parsec.parseString decode "1foo") Nothing

    Spec.it s "fails with empty input" $ do
      Spec.assertEq s (Parsec.parseString decode "") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes a simple name" $ do
      Spec.assertEq s (Builder.toString . encode . MkName $ Text.pack "foo") "foo"

    Spec.it s "encodes a name with hyphens" $ do
      Spec.assertEq s (Builder.toString . encode . MkName $ Text.pack "background-color") "background-color"

    Spec.it s "encodes a vendor-prefixed name" $ do
      Spec.assertEq s (Builder.toString . encode . MkName $ Text.pack "-webkit-transform") "-webkit-transform"
