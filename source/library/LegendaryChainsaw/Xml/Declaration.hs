{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Xml.Declaration where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Extra.Semigroup as Semigroup
import qualified LegendaryChainsaw.Spec as Spec
import qualified LegendaryChainsaw.Xml.Name as Name
import qualified Text.Parsec as Parsec

-- | XML Declaration (for DOCTYPE and similar)
-- <!name value>
-- Similar to Instruction but different delimiters
-- Cannot contain ">"
data Declaration = MkDeclaration
  { name :: Name.Name,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Declaration
decode = Parsec.between (Parsec.string' "<!") (Parsec.char '>') $ do
  n <- Name.decode
  v <-
    Parsec.choice
      [ do
          _ <- Parsec.many1 Parsec.blank
          decodeContent,
        Text.pack "" <$ Parsec.lookAhead (Parsec.char '>')
      ]
  pure $ MkDeclaration n v

decodeContent :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Text.Text
decodeContent = fmap Text.pack . Parsec.manyTill Parsec.anyChar . Parsec.lookAhead $ Parsec.char '>'

encode :: Declaration -> Builder.Builder
encode decl =
  Semigroup.around (Builder.stringUtf8 "<!") (Builder.charUtf8 '>') $
    Name.encode (name decl)
      <> if Text.null (value decl)
        then mempty
        else Builder.charUtf8 ' ' <> Builder.stringUtf8 (Text.unpack $ value decl)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with empty value" $ do
      Spec.assertEq s (Parsec.parseString decode "<!foo>") . Just $
        MkDeclaration (Name.MkName $ Text.pack "foo") (Text.pack "")

    Spec.it s "succeeds with value" $ do
      Spec.assertEq s (Parsec.parseString decode "<!foo bar>") . Just $
        MkDeclaration (Name.MkName $ Text.pack "foo") (Text.pack "bar")

    Spec.it s "succeeds with doctype html" $ do
      Spec.assertEq s (Parsec.parseString decode "<!DOCTYPE html>") . Just $
        MkDeclaration (Name.MkName $ Text.pack "DOCTYPE") (Text.pack "html")

    Spec.it s "succeeds with doctype public" $ do
      Spec.assertEq s (Parsec.parseString decode "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\">") . Just $
        MkDeclaration (Name.MkName $ Text.pack "DOCTYPE") (Text.pack "html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"")

    Spec.it s "fails without closing" $ do
      Spec.assertEq s (Parsec.parseString decode "<!foo") Nothing

    Spec.it s "fails without name" $ do
      Spec.assertEq s (Parsec.parseString decode "<!>") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes empty value" $ do
      Spec.assertEq s (Builder.toString . encode $ MkDeclaration (Name.MkName $ Text.pack "foo") (Text.pack "")) "<!foo>"

    Spec.it s "encodes with value" $ do
      Spec.assertEq s (Builder.toString . encode $ MkDeclaration (Name.MkName $ Text.pack "foo") (Text.pack "bar")) "<!foo bar>"

    Spec.it s "encodes doctype" $ do
      Spec.assertEq s (Builder.toString . encode $ MkDeclaration (Name.MkName $ Text.pack "DOCTYPE") (Text.pack "html")) "<!DOCTYPE html>"
