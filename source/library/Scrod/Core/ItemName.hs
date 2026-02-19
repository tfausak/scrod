{-# LANGUAGE DerivingVia #-}

module Scrod.Core.ItemName where

import qualified Data.Text as Text
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

newtype ItemName = MkItemName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Text.Text

-- | Check whether an item name represents an operator.
--
-- GHC pretty-prints operator names without parentheses (e.g. @>>=@,
-- @+++@), so we detect them by checking whether the first character
-- is a Haskell symbol character.
isOperator :: ItemName -> Bool
isOperator = maybe False (isSymbolChar . fst) . Text.uncons . unwrap

isSymbolChar :: Char -> Bool
isSymbolChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: [Char])
