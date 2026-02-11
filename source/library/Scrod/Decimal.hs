{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Normalized decimal numbers represented as mantissa and exponent.
--
-- A 'Decimal' value represents the number @mantissa * 10 ^ exponent@.
-- The smart constructor 'mkDecimal' ensures a canonical form by stripping
-- trailing zeros from the mantissa (shifting them into the exponent).
module Scrod.Decimal where

import qualified Data.Function as Function
import qualified Scrod.Spec as Spec

data Decimal = MkDecimal
  { mantissa :: Integer,
    exponent :: Integer
  }
  deriving (Eq, Ord, Show)

-- | Construct a 'Decimal' in canonical form. Trailing zeros in the mantissa
-- are stripped and absorbed into the exponent. A zero mantissa always yields
-- @MkDecimal 0 0@ regardless of the exponent.
mkDecimal :: Integer -> Integer -> Decimal
mkDecimal = Function.fix $ \rec m e ->
  if m == 0
    then MkDecimal 0 0
    else
      let (q, r) = quotRem m 10
       in if r == 0
            then rec q (e + 1)
            else MkDecimal m e

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'mkDecimal $ do
    Spec.it s "normalizes zero mantissa" $ do
      Spec.assertEq s (mkDecimal 0 5) $ MkDecimal 0 0

    Spec.it s "normalizes zero mantissa with zero exponent" $ do
      Spec.assertEq s (mkDecimal 0 0) $ MkDecimal 0 0

    Spec.it s "keeps mantissa without trailing zeros" $ do
      Spec.assertEq s (mkDecimal 123 5) $ MkDecimal 123 5

    Spec.it s "removes one trailing zero" $ do
      Spec.assertEq s (mkDecimal 120 5) $ MkDecimal 12 6

    Spec.it s "removes multiple trailing zeros" $ do
      Spec.assertEq s (mkDecimal 12000 5) $ MkDecimal 12 8

    Spec.it s "normalizes negative mantissa with trailing zeros" $ do
      Spec.assertEq s (mkDecimal (-1200) 3) $ MkDecimal (-12) 5

    Spec.it s "keeps negative mantissa without trailing zeros" $ do
      Spec.assertEq s (mkDecimal (-123) 5) $ MkDecimal (-123) 5

    Spec.it s "works with negative exponent" $ do
      Spec.assertEq s (mkDecimal 1200 (-5)) $ MkDecimal 12 (-3)

    Spec.it s "normalizes single digit" $ do
      Spec.assertEq s (mkDecimal 5 0) $ MkDecimal 5 0

    Spec.it s "normalizes mantissa that is all zeros except leading" $ do
      Spec.assertEq s (mkDecimal 1000000 0) $ MkDecimal 1 6
