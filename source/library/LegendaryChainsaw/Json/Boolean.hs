{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Json.Boolean where

import qualified Data.Bool as Bool
import qualified Data.ByteString.Builder as Builder
import qualified LegendaryChainsaw.Extra.Builder as Builder
import qualified LegendaryChainsaw.Extra.Parsec as Parsec
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Parsec as Parsec

newtype Boolean = MkBoolean
  { unwrap :: Bool
  } deriving (Eq, Ord, Show)

decode :: Parsec.Stream s m Char => Parsec.ParsecT s u m Boolean
decode = MkBoolean <$> Parsec.choice
  [ True <$ Parsec.string' "true"
  , False <$ Parsec.string' "false"
  ]

encode :: Boolean -> Builder.Builder
encode = Builder.stringUtf8 . Bool.bool "false" "true" . unwrap

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'decode $ do
    Spec.it s "succeeds with true" $ do
      Spec.assertEq s (Parsec.parseString decode "true") $ Just . MkBoolean $ True

    Spec.it s "succeeds with false" $ do
      Spec.assertEq s (Parsec.parseString decode "false") $ Just . MkBoolean $ False

    Spec.it s "fails with invalid input" $ do
      Spec.assertEq s (Parsec.parseString decode "invalid") Nothing

  Spec.named s 'encode $ do
    Spec.it s "encodes true" $ do
      Spec.assertEq s (Builder.toString . encode . MkBoolean $ True) "true"

    Spec.it s "encodes false" $ do
      Spec.assertEq s (Builder.toString . encode . MkBoolean $ False) "false"
