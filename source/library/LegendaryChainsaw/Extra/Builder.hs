{- hlint ignore "Avoid restricted extensions" -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Extra.Builder where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified LegendaryChainsaw.Spec as Spec

toByteString :: Builder.Builder -> ByteString.ByteString
toByteString = LazyByteString.toStrict . Builder.toLazyByteString

toText :: Builder.Builder -> Text.Text
toText = Encoding.decodeUtf8Lenient . toByteString

toString :: Builder.Builder -> String
toString = Text.unpack . toText

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'toByteString $ do
    Spec.it s "works with an empty builder" $ do
      Spec.assertEq s (toByteString "") ""

    Spec.it s "works with one byte" $ do
      Spec.assertEq s (toByteString " \x0 ") " \x0 "

    Spec.it s "works with two bytes" $ do
      Spec.assertEq s (toByteString " \x80 ") " \xc2\x80 "

    Spec.it s "works with three bytes" $ do
      Spec.assertEq s (toByteString " \x800 ") " \xe0\xa0\x80 "

    Spec.it s "works with four bytes" $ do
      Spec.assertEq s (toByteString " \x10000 ") " \xf0\x90\x80\x80 "

  Spec.named s 'toText $ do
    Spec.it s "works with an empty builder" $ do
      Spec.assertEq s (toText "") ""

    Spec.it s "works with one byte" $ do
      Spec.assertEq s (toText " \x0 ") " \x0 "

    Spec.it s "works with two bytes" $ do
      Spec.assertEq s (toText " \x80 ") " \x80 "

    Spec.it s "works with three bytes" $ do
      Spec.assertEq s (toText " \x800 ") " \x800 "

    Spec.it s "works with four bytes" $ do
      Spec.assertEq s (toText " \x10000 ") " \x10000 "

  Spec.named s 'toString $ do
    Spec.it s "works with an empty builder" $ do
      Spec.assertEq s (toString "") ""

    Spec.it s "works with one byte" $ do
      Spec.assertEq s (toString " \x0 ") " \x0 "

    Spec.it s "works with two bytes" $ do
      Spec.assertEq s (toString " \x80 ") " \x80 "

    Spec.it s "works with three bytes" $ do
      Spec.assertEq s (toString " \x800 ") " \x800 "

    Spec.it s "works with four bytes" $ do
      Spec.assertEq s (toString " \x10000 ") " \x10000 "
