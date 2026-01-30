{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Located where

import qualified Data.Aeson as Aeson
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers
import qualified Scrod.Unstable.Type.Location as Location

data Located a = MkLocated
  { location :: Location.Location,
    value :: a
  }
  deriving (Eq, Ord, Show)

fromJson :: (Aeson.Value -> Either String a) -> Aeson.Value -> Either String (Located a)
fromJson fromJsonA = \case
  Aeson.Object obj -> do
    locJson <- JsonHelpers.lookupField obj "location"
    loc <- Location.fromJson locJson
    valJson <- JsonHelpers.lookupField obj "value"
    val <- fromJsonA valJson
    Right $ MkLocated {location = loc, value = val}
  _ -> Left "Located must be an object"

toJson :: (a -> Aeson.Value) -> Located a -> Aeson.Value
toJson toJsonA (MkLocated loc val) =
  Aeson.object
    [ "location" Aeson..= Location.toJson loc,
      "value" Aeson..= toJsonA val
    ]

fromGhc :: SrcLoc.Located a -> Maybe (Located a)
fromGhc located = do
  location <- Location.fromSrcSpan $ SrcLoc.getLoc located
  pure
    MkLocated
      { location,
        value = SrcLoc.unLoc located
      }
