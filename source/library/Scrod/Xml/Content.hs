{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Content where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Spec as Spec
import qualified Scrod.Xml.Comment as Comment
import qualified Scrod.Xml.Text as XmlText
import qualified Text.Parsec as Parsec

-- | XML Content (what can appear inside an element)
-- Parameterized by element type to avoid circular dependencies.
-- Element.hs uses Content Element.
data Content a
  = Comment Comment.Comment
  | Element a
  | Text Text.Text
  deriving (Eq, Ord, Show)

-- | Decode content, parameterized by element decoder
decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m (Content a)
decode elementParser =
  Parsec.choice
    [ Comment <$> Comment.decode,
      Parsec.try $ Element <$> elementParser,
      Text <$> decodeText
    ]

-- | Decode non-empty text content
decodeText :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Text.Text
decodeText = do
  t <- XmlText.decode
  if Text.null t
    then fail "empty text"
    else pure t

-- | Encode content, parameterized by element encoder
encode :: (a -> Builder.Builder) -> Content a -> Builder.Builder
encode encodeElement c = case c of
  Comment comment -> Comment.encode comment
  Element element -> encodeElement element
  Text text -> XmlText.encode text

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    -- Use a simple element parser for testing
    let elementP :: (Parsec.Stream t m Char) => Parsec.ParsecT t u m String
        elementP = "test" <$ Parsec.string' "<test/>"

    Spec.it s "succeeds with comment" $ do
      Spec.assertEq s (Parsec.parseString (decode elementP) "<!-- hello -->") . Just $
        Comment (Comment.MkComment $ Text.pack " hello ")

    Spec.it s "succeeds with element" $ do
      Spec.assertEq s (Parsec.parseString (decode elementP) "<test/>") . Just $
        Element "test"

    Spec.it s "succeeds with text" $ do
      Spec.assertEq s (Parsec.parseString (decode elementP) "hello") . Just $
        Text (Text.pack "hello")

    Spec.it s "succeeds with text containing entities" $ do
      Spec.assertEq s (Parsec.parseString (decode elementP) "a &amp; b") . Just $
        Text (Text.pack "a & b")

    Spec.it s "fails with empty text" $ do
      Spec.assertEq s (Parsec.parseString (decode elementP) "") Nothing

  Spec.named s 'encode $ do
    let encodeElement :: String -> Builder.Builder
        encodeElement _ = Builder.stringUtf8 "<test/>"

    Spec.it s "encodes comment" $ do
      Spec.assertEq s (Builder.toString . encode encodeElement $ Comment (Comment.MkComment $ Text.pack " hello ")) "<!-- hello -->"

    Spec.it s "encodes element" $ do
      Spec.assertEq s (Builder.toString . encode encodeElement $ Element "test") "<test/>"

    Spec.it s "encodes text" $ do
      Spec.assertEq s (Builder.toString . encode encodeElement $ Text (Text.pack "hello")) "hello"

    Spec.it s "escapes text" $ do
      Spec.assertEq s (Builder.toString . encode encodeElement $ Text (Text.pack "a & b")) "a &amp; b"
