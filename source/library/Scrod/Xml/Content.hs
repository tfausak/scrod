{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Content where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Spec as Spec
import qualified Scrod.Xml.Comment as Comment
import qualified Scrod.Xml.Text as XmlText

-- | XML Content (what can appear inside an element). Parameterized by element
-- type to avoid circular dependencies. "Scrod.Xml.Element" uses @'Content'
-- Element@.
data Content a
  = Comment Comment.Comment
  | Element a
  | Raw Text.Text
  | Text Text.Text
  deriving (Eq, Ord, Show)

-- | Encode content, parameterized by element encoder.
encode :: (a -> Builder.Builder) -> Content a -> Builder.Builder
encode encodeElement c = case c of
  Comment comment -> Comment.encode comment
  Element element -> encodeElement element
  Raw raw -> foldMap Builder.charUtf8 (Text.unpack raw)
  Text text -> XmlText.encode text

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
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

    Spec.it s "encodes raw" $ do
      Spec.assertEq s (Builder.toString . encode encodeElement $ Raw (Text.pack "hello")) "hello"

    Spec.it s "does not escape raw" $ do
      Spec.assertEq s (Builder.toString . encode encodeElement $ Raw (Text.pack "a > b & c < d")) "a > b & c < d"
