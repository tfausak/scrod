{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Namespace where

import qualified Scrod.Unstable.Type.Json as Json

-- | The namespace qualification for an identifier.
-- Mirrors 'Documentation.Haddock.Types.Namespace' from haddock-library,
-- but without the 'None' case (represented as 'Nothing' in 'Identifier').
data Namespace
  = -- | v'identifier' syntax
    Value
  | -- | t'identifier' syntax
    Type
  deriving (Eq, Ord, Show)

toJson :: Namespace -> Json.Json
toJson ns = case ns of
  Value -> Json.tag "Value"
  Type -> Json.tag "Type"
