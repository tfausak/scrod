{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Css.Declaration where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Css.Name as Name
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Spec as Spec
import qualified Text.Parsec as Parsec

-- | CSS Declaration
-- property: value or property: value !important
data Declaration = MkDeclaration
  { property :: Name.Name,
    value :: Text.Text,
    important :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Decode a declaration
-- property : value [!important] [;]
decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Declaration
decode = do
  _ <- Parsec.many Parsec.blank
  prop <- Name.decode
  _ <- Parsec.many Parsec.blank
  _ <- Parsec.char ':'
  _ <- Parsec.many Parsec.blank
  (val, imp) <- decodeValue
  _ <- Parsec.many Parsec.blank
  _ <- Parsec.optional $ Parsec.char ';'
  pure $ MkDeclaration prop val imp

-- | Decode a value, returning (value, isImportant)
-- Handles quoted strings to correctly find the end
decodeValue :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m (Text.Text, Bool)
decodeValue = do
  chars <- decodeValueChars
  let raw = Text.strip . Text.pack $ chars
  let (val, imp) = extractImportant raw
  if Text.null val
    then fail "empty value"
    else pure (val, imp)

-- | Decode value characters, handling quoted strings
decodeValueChars :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m String
decodeValueChars = concat <$> Parsec.many decodeValuePart

decodeValuePart :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m String
decodeValuePart =
  Parsec.choice
    [ decodeQuotedString '"',
      decodeQuotedString '\'',
      (: []) <$> Parsec.satisfy (\c -> c /= ';' && c /= '}' && c /= '"' && c /= '\'')
    ]

-- | Decode a quoted string including the quotes
decodeQuotedString :: (Parsec.Stream s m Char) => Char -> Parsec.ParsecT s u m String
decodeQuotedString q = do
  _ <- Parsec.char q
  chars <- Parsec.many $ Parsec.choice [Parsec.try (Parsec.string ['\\', q]), (: []) <$> Parsec.satisfy (/= q)]
  _ <- Parsec.char q
  pure $ q : (concat chars <> [q])

-- | Extract !important suffix from value
extractImportant :: Text.Text -> (Text.Text, Bool)
extractImportant t =
  let lower = Text.toLower t
   in if Text.pack "!important" `Text.isSuffixOf` lower
        then (Text.strip $ Text.dropEnd 10 t, True)
        else (t, False)

encode :: Declaration -> Builder.Builder
encode decl =
  Name.encode (property decl)
    <> Builder.stringUtf8 ": "
    <> Builder.stringUtf8 (Text.unpack $ value decl)
    <> (if important decl then Builder.stringUtf8 " !important" else mempty)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with simple declaration" $ do
      Spec.assertEq s (Parsec.parseString decode "color: red") . Just $
        MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False

    Spec.it s "succeeds with semicolon" $ do
      Spec.assertEq s (Parsec.parseString decode "color: red;") . Just $
        MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False

    Spec.it s "succeeds with !important" $ do
      Spec.assertEq s (Parsec.parseString decode "color: red !important") . Just $
        MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") True

    Spec.it s "succeeds with !important and semicolon" $ do
      Spec.assertEq s (Parsec.parseString decode "color: red !important;") . Just $
        MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") True

    Spec.it s "succeeds with spaces around colon" $ do
      Spec.assertEq s (Parsec.parseString decode "color : red") . Just $
        MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False

    Spec.it s "succeeds with complex value" $ do
      Spec.assertEq s (Parsec.parseString decode "font: 12px/1.5 Arial, sans-serif") . Just $
        MkDeclaration (Name.MkName $ Text.pack "font") (Text.pack "12px/1.5 Arial, sans-serif") False

    Spec.it s "succeeds with url value" $ do
      Spec.assertEq s (Parsec.parseString decode "background: url(\"image.png\")") . Just $
        MkDeclaration (Name.MkName $ Text.pack "background") (Text.pack "url(\"image.png\")") False

    Spec.it s "succeeds with quoted string containing semicolon" $ do
      Spec.assertEq s (Parsec.parseString decode "content: \"hello; world\"") . Just $
        MkDeclaration (Name.MkName $ Text.pack "content") (Text.pack "\"hello; world\"") False

    Spec.it s "succeeds with vendor prefix" $ do
      Spec.assertEq s (Parsec.parseString decode "-webkit-transform: rotate(45deg)") . Just $
        MkDeclaration (Name.MkName $ Text.pack "-webkit-transform") (Text.pack "rotate(45deg)") False

    Spec.it s "succeeds with leading whitespace" $ do
      Spec.assertEq s (Parsec.parseString decode "  color: red") . Just $
        MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False

    Spec.it s "fails with empty value" $ do
      Spec.assertEq s (Parsec.parseString decode "color:") Nothing

    Spec.it s "fails without colon" $ do
      Spec.assertEq s (Parsec.parseString decode "color red") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes simple declaration" $ do
      Spec.assertEq
        s
        (Builder.toString . encode $ MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") False)
        "color: red"

    Spec.it s "encodes with !important" $ do
      Spec.assertEq
        s
        (Builder.toString . encode $ MkDeclaration (Name.MkName $ Text.pack "color") (Text.pack "red") True)
        "color: red !important"

    Spec.it s "encodes complex value" $ do
      Spec.assertEq
        s
        (Builder.toString . encode $ MkDeclaration (Name.MkName $ Text.pack "font") (Text.pack "12px Arial") False)
        "font: 12px Arial"
