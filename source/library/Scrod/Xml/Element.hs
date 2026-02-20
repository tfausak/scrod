{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Element where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Spec as Spec
import qualified Scrod.Xml.Attribute as Attribute
import qualified Scrod.Xml.Content as Content
import qualified Scrod.Xml.Name as Name

-- | XML Element, like @\<name attr="value">content\</name>@. Can be
-- self-closing: @\<name />@.
data Element = MkElement
  { name :: Name.Name,
    attributes :: [Attribute.Attribute],
    contents :: [Content.Content Element]
  }
  deriving (Eq, Ord, Show)

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

-- | Encode an element as HTML. Only void elements are self-closing; all
-- other elements use open and close tags even when empty. Recursively
-- fills empty non-void elements with an empty text node so that the
-- regular XML encoder produces paired tags.
encodeHtml :: Element -> Builder.Builder
encodeHtml = encode . fillNonVoidElements

fillNonVoidElements :: Element -> Element
fillNonVoidElements el =
  let cs = fmap fillNonVoidContent (contents el)
   in if null cs && not (isHtmlVoidElement (Name.unwrap $ name el))
        then el {contents = [Content.Text Text.empty]}
        else el {contents = cs}

fillNonVoidContent :: Content.Content Element -> Content.Content Element
fillNonVoidContent c = case c of
  Content.Element e -> Content.Element (fillNonVoidElements e)
  _ -> c

-- | Whether a tag name is an HTML void element that may be self-closing.
-- See <https://html.spec.whatwg.org/multipage/syntax.html#void-elements>.
isHtmlVoidElement :: Text.Text -> Bool
isHtmlVoidElement = (`Set.member` htmlVoidElements)

htmlVoidElements :: Set.Set Text.Text
htmlVoidElements =
  Set.fromList $
    fmap
      Text.pack
      [ "area",
        "base",
        "br",
        "col",
        "embed",
        "hr",
        "img",
        "input",
        "link",
        "meta",
        "source",
        "track",
        "wbr"
      ]

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  let mkName :: String -> Name.Name
      mkName = Name.MkName . Text.pack
      mkAttr :: String -> String -> Attribute.Attribute
      mkAttr n v = Attribute.MkAttribute (mkName n) (Text.pack v)
      mkText :: String -> Content.Content Element
      mkText = Content.Text . Text.pack
      mkElement :: String -> [Attribute.Attribute] -> [Content.Content Element] -> Content.Content Element
      mkElement n as cs = Content.Element $ MkElement (mkName n) as cs

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

  Spec.named s 'encodeHtml $ do
    Spec.it s "self-closes void elements" $ do
      Spec.assertEq s (Builder.toString . encodeHtml $ MkElement (mkName "br") [] []) "<br />"

    Spec.it s "self-closes void elements with attributes" $ do
      Spec.assertEq s (Builder.toString . encodeHtml $ MkElement (mkName "img") [mkAttr "src" "x.png"] []) "<img src=\"x.png\" />"

    Spec.it s "uses paired tags for non-void elements when empty" $ do
      Spec.assertEq s (Builder.toString . encodeHtml $ MkElement (mkName "div") [] []) "<div></div>"

    Spec.it s "uses paired tags for non-void elements with content" $ do
      Spec.assertEq s (Builder.toString . encodeHtml $ MkElement (mkName "div") [] [mkText "hello"]) "<div>hello</div>"

    Spec.it s "applies recursively to nested elements" $ do
      Spec.assertEq s (Builder.toString . encodeHtml $ MkElement (mkName "div") [] [mkElement "span" [] []]) "<div><span></span></div>"

    Spec.it s "applies recursively to nested void elements" $ do
      Spec.assertEq s (Builder.toString . encodeHtml $ MkElement (mkName "div") [] [mkElement "br" [] []]) "<div><br /></div>"
