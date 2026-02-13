{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Document where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Spec as Spec
import qualified Scrod.Xml.Attribute as Attribute
import qualified Scrod.Xml.Content as Content
import qualified Scrod.Xml.Declaration as Declaration
import qualified Scrod.Xml.Element as Element
import qualified Scrod.Xml.Instruction as Instruction
import qualified Scrod.Xml.Misc as Misc
import qualified Scrod.Xml.Name as Name

-- | XML Document. Prolog (misc items) followed by root element. No epilog
-- support.
data Document = MkDocument
  { prolog :: [Misc.Misc],
    root :: Element.Element
  }
  deriving (Eq, Ord, Show)

element :: String -> [Attribute.Attribute] -> [Content.Content Element.Element] -> Element.Element
element name attributes contents =
  Element.MkElement
    { Element.name = Name.MkName $ Text.pack name,
      Element.attributes = attributes,
      Element.contents = contents
    }

attribute :: String -> String -> Attribute.Attribute
attribute name value =
  Attribute.MkAttribute
    { Attribute.name = Name.MkName $ Text.pack name,
      Attribute.value = Text.pack value
    }

raw :: Text.Text -> Content.Content a
raw = Content.Raw

string :: String -> Content.Content a
string = text . Text.pack

text :: Text.Text -> Content.Content a
text = Content.Text

encode :: Document -> Builder.Builder
encode doc =
  foldMap (\m -> Misc.encode m <> Builder.charUtf8 '\n') (prolog doc)
    <> Element.encode (root doc)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  let mkName :: String -> Name.Name
      mkName = Name.MkName . Text.pack
      mkText :: String -> Content.Content Element.Element
      mkText = Content.Text . Text.pack
      mkElement :: String -> [Content.Content Element.Element] -> Element.Element
      mkElement n = Element.MkElement (mkName n) []

  Spec.named s 'encode $ do
    Spec.it s "encodes simple document" $ do
      Spec.assertEq s (Builder.toString . encode $ MkDocument [] (mkElement "root" [])) "<root />"

    Spec.it s "encodes document with xml declaration" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode $
            MkDocument
              [Misc.Instruction $ Instruction.MkInstruction (mkName "xml") (Text.pack "version=\"1.0\"")]
              (mkElement "root" [])
        )
        "<?xml version=\"1.0\"?>\n<root />"

    Spec.it s "encodes document with multiple prolog items" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode $
            MkDocument
              [ Misc.Instruction $ Instruction.MkInstruction (mkName "xml") (Text.pack "version=\"1.0\""),
                Misc.Declaration $ Declaration.MkDeclaration (mkName "DOCTYPE") (Text.pack "html")
              ]
              (mkElement "html" [])
        )
        "<?xml version=\"1.0\"?>\n<!DOCTYPE html>\n<html />"

    Spec.it s "encodes document with content" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode $
            MkDocument
              []
              (mkElement "root" [mkText "hello"])
        )
        "<root>hello</root>"

    Spec.it s "encodes raw content without escaping" $ do
      Spec.assertEq
        s
        ( Builder.toString . encode $
            MkDocument
              []
              (mkElement "style" [Content.Raw $ Text.pack "a > b { color: red; }"])
        )
        "<style>a > b { color: red; }</style>"
