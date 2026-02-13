{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Text where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Spec as Spec

encode :: Text.Text -> Builder.Builder
encode = foldMap encodeChar . Text.unpack

encodeChar :: Char -> Builder.Builder
encodeChar c = case c of
  '&' -> Builder.stringUtf8 "&amp;"
  '<' -> Builder.stringUtf8 "&lt;"
  '>' -> Builder.stringUtf8 "&gt;"
  _ -> Builder.charUtf8 c

-- | Encode for attribute values (also escapes quotes)
encodeAttr :: Text.Text -> Builder.Builder
encodeAttr = foldMap encodeAttrChar . Text.unpack

encodeAttrChar :: Char -> Builder.Builder
encodeAttrChar c = case c of
  '\'' -> Builder.stringUtf8 "&apos;"
  '"' -> Builder.stringUtf8 "&quot;"
  _ -> encodeChar c

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'encode $ do
    Spec.it s "encodes empty text" $ do
      Spec.assertEq s (Builder.toString . encode $ Text.pack "") ""

    Spec.it s "encodes plain text" $ do
      Spec.assertEq s (Builder.toString . encode $ Text.pack "hello") "hello"

    Spec.it s "encodes ampersand" $ do
      Spec.assertEq s (Builder.toString . encode $ Text.pack "a & b") "a &amp; b"

    Spec.it s "encodes less-than" $ do
      Spec.assertEq s (Builder.toString . encode $ Text.pack "a < b") "a &lt; b"

    Spec.it s "encodes greater-than" $ do
      Spec.assertEq s (Builder.toString . encode $ Text.pack "a > b") "a &gt; b"

  Spec.named s 'encodeAttr $ do
    Spec.it s "encodes single quote" $ do
      Spec.assertEq s (Builder.toString . encodeAttr $ Text.pack "a ' b") "a &apos; b"

    Spec.it s "encodes double quote" $ do
      Spec.assertEq s (Builder.toString . encodeAttr $ Text.pack "a \" b") "a &quot; b"
