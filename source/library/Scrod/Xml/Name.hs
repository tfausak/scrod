{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Name where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Spec as Spec

-- | XML Name (used for elements, attributes, and instructions).
newtype Name = MkName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

encode :: Name -> Builder.Builder
encode = Builder.stringUtf8 . Text.unpack . unwrap

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'encode $ do
    Spec.it s "encodes a simple name" $ do
      Spec.assertEq s (Builder.toString . encode . MkName $ Text.pack "foo") "foo"

    Spec.it s "encodes a name with special chars" $ do
      Spec.assertEq s (Builder.toString . encode . MkName $ Text.pack "foo-bar_123") "foo-bar_123"
