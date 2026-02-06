{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Attribute where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Extra.Semigroup as Semigroup
import qualified Scrod.Spec as Spec
import qualified Scrod.Xml.Name as Name
import qualified Scrod.Xml.Text as XmlText
import qualified Text.Parsec as Parsec

-- | XML Attribute
-- name="value" or name='value'
-- Can have spaces around equal sign
data Attribute = MkAttribute
  { name :: Name.Name,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Attribute
decode =
  MkAttribute
    <$> (Name.decode <* Parsec.many Parsec.blank)
    <*> (Parsec.char '=' *> Parsec.many Parsec.blank *> decodeValue)

decodeValue :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Text.Text
decodeValue =
  Parsec.choice
    [ decodeQuotedValue '"',
      decodeQuotedValue '\''
    ]

decodeQuotedValue :: (Parsec.Stream s m Char) => Char -> Parsec.ParsecT s u m Text.Text
decodeQuotedValue q = Parsec.between (Parsec.char q) (Parsec.char q) $ decodeValueContent q

decodeValueContent :: (Parsec.Stream s m Char) => Char -> Parsec.ParsecT s u m Text.Text
decodeValueContent q = Text.pack <$> Parsec.many (decodeValueChar q)

decodeValueChar :: (Parsec.Stream s m Char) => Char -> Parsec.ParsecT s u m Char
decodeValueChar q =
  Parsec.choice
    [ XmlText.decodeEntity,
      Parsec.satisfy $ \c -> c /= '<' && c /= '&' && c /= q
    ]

encode :: Attribute -> Builder.Builder
encode attr =
  Name.encode (name attr)
    <> Builder.charUtf8 '='
    <> Semigroup.around (Builder.charUtf8 '"') (Builder.charUtf8 '"') (XmlText.encodeAttr $ value attr)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with double quotes" $ do
      Spec.assertEq s (Parsec.parseString decode "foo=\"bar\"") . Just $
        MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "bar")

    Spec.it s "succeeds with single quotes" $ do
      Spec.assertEq s (Parsec.parseString decode "foo='bar'") . Just $
        MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "bar")

    Spec.it s "succeeds with empty value" $ do
      Spec.assertEq s (Parsec.parseString decode "foo=\"\"") . Just $
        MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "")

    Spec.it s "succeeds with spaces around equals" $ do
      Spec.assertEq s (Parsec.parseString decode "foo = \"bar\"") . Just $
        MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "bar")

    Spec.it s "succeeds with entity in value" $ do
      Spec.assertEq s (Parsec.parseString decode "foo=\"a &amp; b\"") . Just $
        MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "a & b")

    Spec.it s "succeeds with numeric entity" $ do
      Spec.assertEq s (Parsec.parseString decode "foo=\"&#65;\"") . Just $
        MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "A")

    Spec.it s "succeeds with single quote in double-quoted value" $ do
      Spec.assertEq s (Parsec.parseString decode "foo=\"a ' b\"") . Just $
        MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "a ' b")

    Spec.it s "succeeds with double quote in single-quoted value" $ do
      Spec.assertEq s (Parsec.parseString decode "foo='a \" b'") . Just $
        MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "a \" b")

    Spec.it s "fails without quotes" $ do
      Spec.assertEq s (Parsec.parseString decode "foo=bar") Nothing

    Spec.it s "fails without value" $ do
      Spec.assertEq s (Parsec.parseString decode "foo=") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes simple attribute" $ do
      Spec.assertEq s (Builder.toString . encode $ MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "bar")) "foo=\"bar\""

    Spec.it s "encodes empty value" $ do
      Spec.assertEq s (Builder.toString . encode $ MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "")) "foo=\"\""

    Spec.it s "escapes ampersand in value" $ do
      Spec.assertEq s (Builder.toString . encode $ MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "a & b")) "foo=\"a &amp; b\""

    Spec.it s "escapes quote in value" $ do
      Spec.assertEq s (Builder.toString . encode $ MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "a \" b")) "foo=\"a &quot; b\""

    Spec.it s "escapes less-than in value" $ do
      Spec.assertEq s (Builder.toString . encode $ MkAttribute (Name.MkName $ Text.pack "foo") (Text.pack "a < b")) "foo=\"a &lt; b\""
