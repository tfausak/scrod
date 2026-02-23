{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Extra.Builder where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Scrod.Spec as Spec

toByteString :: Builder.Builder -> ByteString.ByteString
toByteString = LazyByteString.toStrict . Builder.toLazyByteString

toText :: Builder.Builder -> Text.Text
toText = Encoding.decodeUtf8Lenient . toByteString

toString :: Builder.Builder -> String
toString = Text.unpack . toText

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'toByteString $ do
    let f = toByteString . Builder.stringUtf8
    let b = ByteString.pack

    Spec.it s "works with an empty builder" $ do
      Spec.assertEq s (f "") $ b []

    Spec.it s "works with one byte" $ do
      Spec.assertEq s (f " \x0 ") $ b [0x20, 0x00, 0x20]

    Spec.it s "works with two bytes" $ do
      Spec.assertEq s (f " \x80 ") $ b [0x20, 0xc2, 0x80, 0x20]

    Spec.it s "works with three bytes" $ do
      Spec.assertEq s (f " \x800 ") $ b [0x20, 0xe0, 0xa0, 0x80, 0x20]

    Spec.it s "works with four bytes" $ do
      Spec.assertEq s (f " \x10000 ") $ b [0x20, 0xf0, 0x90, 0x80, 0x80, 0x20]

  Spec.named s 'toText $ do
    let f = toText . Builder.stringUtf8
    let t = Text.pack

    Spec.it s "works with an empty builder" $ do
      Spec.assertEq s (f "") $ t ""

    Spec.it s "works with one byte" $ do
      Spec.assertEq s (f " \x0 ") $ t " \x0 "

    Spec.it s "works with two bytes" $ do
      Spec.assertEq s (f " \x80 ") $ t " \x80 "

    Spec.it s "works with three bytes" $ do
      Spec.assertEq s (f " \x800 ") $ t " \x800 "

    Spec.it s "works with four bytes" $ do
      Spec.assertEq s (f " \x10000 ") $ t " \x10000 "

  Spec.named s 'toString $ do
    let f = toString . Builder.stringUtf8

    Spec.it s "works with an empty builder" $ do
      Spec.assertEq s (f "") ""

    Spec.it s "works with one byte" $ do
      Spec.assertEq s (f " \x0 ") " \x0 "

    Spec.it s "works with two bytes" $ do
      Spec.assertEq s (f " \x80 ") " \x80 "

    Spec.it s "works with three bytes" $ do
      Spec.assertEq s (f " \x800 ") " \x800 "

    Spec.it s "works with four bytes" $ do
      Spec.assertEq s (f " \x10000 ") " \x10000 "
