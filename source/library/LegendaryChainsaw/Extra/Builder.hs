{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Extra.Builder where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified LegendaryChainsaw.Spec as Spec

toString :: Builder.Builder -> String
toString = LazyByteString.unpack . Builder.toLazyByteString

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'toString $ do
    Spec.it s "works with an empty builder" $ do
      Spec.assertEq s (toString mempty) ""

    Spec.it s "works with one byte" $ do
      Spec.assertEq s (toString $ Builder.stringUtf8 " \x0 ") " \x0 "

    Spec.it s "works with two bytes" $ do
      Spec.assertEq s (toString $ Builder.stringUtf8 " \x80 ") " \xc2\x80 "

    Spec.it s "works with three bytes" $ do
      Spec.assertEq s (toString $ Builder.stringUtf8 " \x800 ") " \xe0\xa0\x80 "

    Spec.it s "works with four bytes" $ do
      Spec.assertEq s (toString $ Builder.stringUtf8 " \x10000 ") " \xf0\x90\x80\x80 "
