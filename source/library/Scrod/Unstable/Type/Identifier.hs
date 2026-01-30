{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Identifier where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json
import qualified Scrod.Unstable.Type.Namespace as Namespace

-- | An identifier reference in documentation.
-- Combines an optional namespace (Value or Type) with the identifier text.
-- 'Nothing' namespace corresponds to plain 'identifier' syntax.
data Identifier = MkIdentifier
  { namespace :: Maybe Namespace.Namespace,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)

toJson :: Identifier -> Json.Json
toJson (MkIdentifier ns v) =
  Json.object
    [ ("namespace", maybe Json.Null Namespace.toJson ns),
      ("value", Json.fromText v)
    ]
