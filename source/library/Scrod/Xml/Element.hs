{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Element where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Spec as Spec
import qualified Scrod.Xml.Attribute as Attribute
import qualified Scrod.Xml.Comment as Comment
import qualified Scrod.Xml.Content as Content
import qualified Scrod.Xml.Name as Name
import qualified Text.Parsec as Parsec

-- | XML Element, like @\<name attr="value">content\</name>@. Can be
-- self-closing: @\<name />@.
data Element = MkElement
  { name :: Name.Name,
    attributes :: [Attribute.Attribute],
    contents :: [Content.Content Element]
  }
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Element
decode = do
  _ <- Parsec.char '<'
  n <- Name.decode
  attrs <- Parsec.many $ Parsec.try (Parsec.many1 Parsec.blank *> Attribute.decode)
  _ <- Parsec.many Parsec.blank
  Parsec.choice
    [ do
        -- Self-closing tag
        _ <- Parsec.string' "/>"
        pure $ MkElement n attrs [],
      do
        -- Opening tag with contents
        _ <- Parsec.char '>'
        cs <- Parsec.many $ Content.decode decode
        closeName <- Parsec.string' "</" *> Name.decode <* Parsec.many Parsec.blank <* Parsec.char '>'
        Monad.when (n /= closeName) $ fail "mismatched closing tag"
        pure $ MkElement n attrs cs
    ]

encode :: Element -> Builder.Builder
encode el =
  if null (contents el)
    then encodeSelfClosing el
    else encodeWithContents el

encodeSelfClosing :: Element -> Builder.Builder
encodeSelfClosing el =
  Builder.charUtf8 '<'
    <> Name.encode (name el)
    <> encodeAttributes (attributes el)
    <> Builder.stringUtf8 " />"

encodeWithContents :: Element -> Builder.Builder
encodeWithContents el =
  Builder.charUtf8 '<'
    <> Name.encode (name el)
    <> encodeAttributes (attributes el)
    <> Builder.charUtf8 '>'
    <> foldMap (Content.encode encode) (contents el)
    <> Builder.stringUtf8 "</"
    <> Name.encode (name el)
    <> Builder.charUtf8 '>'

encodeAttributes :: [Attribute.Attribute] -> Builder.Builder
encodeAttributes = foldMap (\a -> Builder.charUtf8 ' ' <> Attribute.encode a)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  let mkName :: String -> Name.Name
      mkName = Name.MkName . Text.pack
      mkAttr :: String -> String -> Attribute.Attribute
      mkAttr n v = Attribute.MkAttribute (mkName n) (Text.pack v)
      mkText :: String -> Content.Content Element
      mkText = Content.Text . Text.pack
      mkComment :: String -> Content.Content Element
      mkComment = Content.Comment . Comment.MkComment . Text.pack
      mkElement :: String -> [Attribute.Attribute] -> [Content.Content Element] -> Content.Content Element
      mkElement n as cs = Content.Element $ MkElement (mkName n) as cs

  Spec.named s 'decode $ do
    Spec.it s "succeeds with self-closing tag" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo/>") . Just $
        MkElement (mkName "foo") [] []

    Spec.it s "succeeds with self-closing tag with space" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo />") . Just $
        MkElement (mkName "foo") [] []

    Spec.it s "succeeds with empty element" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo></foo>") . Just $
        MkElement (mkName "foo") [] []

    Spec.it s "succeeds with text content" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo>bar</foo>") . Just $
        MkElement (mkName "foo") [] [mkText "bar"]

    Spec.it s "succeeds with attribute" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo bar=\"baz\"/>") . Just $
        MkElement (mkName "foo") [mkAttr "bar" "baz"] []

    Spec.it s "succeeds with multiple attributes" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo a=\"1\" b=\"2\"/>") . Just $
        MkElement (mkName "foo") [mkAttr "a" "1", mkAttr "b" "2"] []

    Spec.it s "succeeds with nested element" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo><bar/></foo>") . Just $
        MkElement (mkName "foo") [] [mkElement "bar" [] []]

    Spec.it s "succeeds with mixed content" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo>text<bar/>more</foo>") . Just $
        MkElement (mkName "foo") [] [mkText "text", mkElement "bar" [] [], mkText "more"]

    Spec.it s "succeeds with comment in content" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo><!-- hi --></foo>") . Just $
        MkElement (mkName "foo") [] [mkComment " hi "]

    Spec.it s "succeeds with entity in text" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo>&lt;bar&gt;</foo>") . Just $
        MkElement (mkName "foo") [] [Content.Text $ Text.pack "<bar>"]

    Spec.it s "fails with mismatched tags" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo></bar>") Nothing

    Spec.it s "fails with unclosed tag" $ do
      Spec.assertEq s (Parsec.parseString decode "<foo>") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes self-closing tag" $ do
      Spec.assertEq s (Builder.toString . encode $ MkElement (mkName "foo") [] []) "<foo />"

    Spec.it s "encodes with text content" $ do
      Spec.assertEq s (Builder.toString . encode $ MkElement (mkName "foo") [] [mkText "bar"]) "<foo>bar</foo>"

    Spec.it s "encodes with attribute" $ do
      Spec.assertEq s (Builder.toString . encode $ MkElement (mkName "foo") [mkAttr "bar" "baz"] []) "<foo bar=\"baz\" />"

    Spec.it s "encodes with multiple attributes" $ do
      Spec.assertEq s (Builder.toString . encode $ MkElement (mkName "foo") [mkAttr "a" "1", mkAttr "b" "2"] []) "<foo a=\"1\" b=\"2\" />"

    Spec.it s "encodes nested elements" $ do
      Spec.assertEq s (Builder.toString . encode $ MkElement (mkName "foo") [] [mkElement "bar" [] []]) "<foo><bar /></foo>"

    Spec.it s "escapes text content" $ do
      Spec.assertEq s (Builder.toString . encode $ MkElement (mkName "foo") [] [Content.Text $ Text.pack "a & b"]) "<foo>a &amp; b</foo>"
