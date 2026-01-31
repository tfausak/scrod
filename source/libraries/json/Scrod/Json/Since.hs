{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Since where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.PackageName as PackageName
import qualified Scrod.Json.Version as Version
import qualified Scrod.Type.Since as Type

fromJson :: Aeson.Value -> Either String Type.Since
fromJson value = case value of
  Aeson.Object obj -> do
    pkgJson <- Helpers.lookupField obj "package"
    pkg <- case pkgJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (PackageName.fromJson pkgJson)
    verJson <- Helpers.lookupField obj "version"
    ver <- Version.fromJson verJson
    Right $ Type.MkSince {Type.package = pkg, Type.version = ver}
  _ -> Left "Since must be an object"

toJson :: Type.Since -> Aeson.Value
toJson (Type.MkSince pkg ver) =
  Aeson.object
    [ "package" Aeson..= maybe Aeson.Null PackageName.toJson pkg,
      "version" Aeson..= Version.toJson ver
    ]
