{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Attribute where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Semigroup as Semigroup
import qualified Scrod.Spec as Spec
import qualified Scrod.Xml.Name as Name
import qualified Scrod.Xml.Text as XmlText

-- | XML Attribute
-- name="value" or name='value'
-- Can have spaces around equal sign
data Attribute = MkAttribute
  { name :: Name.Name,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)

encode :: Attribute -> Builder.Builder
encode attr =
  Name.encode (name attr)
    <> Builder.charUtf8 '='
    <> Semigroup.around (Builder.charUtf8 '"') (Builder.charUtf8 '"') (XmlText.encodeAttr $ value attr)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
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
