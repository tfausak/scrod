{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Subordinates where

import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Scrod.Unstable.Type.ExportName as ExportName
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers

-- | Subordinate exports for a type or class.
-- Represents the contents of parentheses in exports like @Foo(..)@ or @Foo(Bar, Baz)@.
data Subordinates = MkSubordinates
  { -- | Whether a @(..)@ wildcard is present.
    wildcard :: Bool,
    -- | Explicitly listed children.
    explicit :: [ExportName.ExportName]
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Subordinates
fromJson = \case
  Aeson.Object obj -> do
    wildcardJson <- JsonHelpers.lookupField obj "wildcard"
    w <- case wildcardJson of
      Aeson.Bool b -> Right b
      _ -> Left "wildcard must be a boolean"
    explicitJson <- JsonHelpers.lookupField obj "explicit"
    e <- case explicitJson of
      Aeson.Array vec -> traverse ExportName.fromJson (Vector.toList vec)
      _ -> Left "explicit must be an array"
    Right $ MkSubordinates {wildcard = w, explicit = e}
  _ -> Left "Subordinates must be an object"

toJson :: Subordinates -> Aeson.Value
toJson (MkSubordinates w e) =
  Aeson.object
    [ "wildcard" Aeson..= w,
      "explicit" Aeson..= fmap ExportName.toJson e
    ]
