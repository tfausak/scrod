{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Xml.Comment where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Semigroup as Semigroup
import qualified Scrod.Spec as Spec

-- | XML Comment like @\<!-- comment text -->@. Cannot contain @-->@.
newtype Comment = MkComment
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

encode :: Comment -> Builder.Builder
encode =
  Semigroup.around (Builder.stringUtf8 "<!--") (Builder.stringUtf8 "-->")
    . Builder.stringUtf8
    . Text.unpack
    . unwrap

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'encode $ do
    Spec.it s "encodes empty comment" $ do
      Spec.assertEq s (Builder.toString . encode . MkComment $ Text.pack "") "<!---->"

    Spec.it s "encodes comment with text" $ do
      Spec.assertEq s (Builder.toString . encode . MkComment $ Text.pack " hello ") "<!-- hello -->"

    Spec.it s "encodes comment with special chars" $ do
      Spec.assertEq s (Builder.toString . encode . MkComment $ Text.pack " & ") "<!-- & -->"
