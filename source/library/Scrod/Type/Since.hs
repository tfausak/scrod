{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Type.Since where

import qualified Data.Aeson as Aeson
import qualified Scrod.Type.JsonHelpers as JsonHelpers
import qualified Scrod.Type.PackageName as PackageName
import qualified Scrod.Type.Version as Version

data Since = MkSince
  { package :: Maybe PackageName.PackageName,
    version :: Version.Version
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Since
fromJson = \case
  Aeson.Object obj -> do
    pkgJson <- JsonHelpers.lookupField obj "package"
    pkg <- case pkgJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (PackageName.fromJson pkgJson)
    verJson <- JsonHelpers.lookupField obj "version"
    ver <- Version.fromJson verJson
    Right $ MkSince {package = pkg, version = ver}
  _ -> Left "Since must be an object"

toJson :: Since -> Aeson.Value
toJson (MkSince pkg ver) =
  Aeson.object
    [ "package" Aeson..= maybe Aeson.Null PackageName.toJson pkg,
      "version" Aeson..= Version.toJson ver
    ]
