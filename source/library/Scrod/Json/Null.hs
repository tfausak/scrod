{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Json.Null where

import qualified Data.ByteString.Builder as Builder
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Spec as Spec
import qualified Text.Parsec as Parsec

data Null
  = MkNull
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Null
decode = MkNull <$ Parsec.string' "null"

encode :: Null -> Builder.Builder
encode = const $ Builder.stringUtf8 "null"

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with valid input" $ do
      Spec.assertEq s (Parsec.parseString decode "null") $ Just MkNull

    Spec.it s "fails with invalid input" $ do
      Spec.assertEq s (Parsec.parseString decode "invalid") Nothing

  Spec.named s 'encode $ do
    Spec.it s "works" $ do
      Spec.assertEq s (Builder.toString $ encode MkNull) "null"
