module Scrod.Unstable.Type.Decimal where

-- | Arbitrary precision decimal number.
-- Represented as coefficient * 10^exponent.
-- Examples:
--   123.45 = MkDecimal { coefficient = 12345, exponent = -2 }
--   1.0e10 = MkDecimal { coefficient = 1, exponent = 10 }
data Decimal = MkDecimal
  { coefficient :: Integer,
    exponent :: Integer
  }
  deriving (Eq, Ord, Show)
