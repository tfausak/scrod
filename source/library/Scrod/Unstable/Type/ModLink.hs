{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.ModLink where

import qualified Data.Aeson as Aeson
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers
import qualified Scrod.Unstable.Type.ModuleName as ModuleName

-- | A link to a module with an optional label.
-- Mirrors 'Documentation.Haddock.Types.ModLink' from haddock-library,
-- but uses 'ModuleName' instead of 'String' for the module name.
data ModLink doc = MkModLink
  { name :: ModuleName.ModuleName,
    label :: Maybe doc
  }
  deriving (Eq, Ord, Show)

fromJson :: (Aeson.Value -> Either String doc) -> Aeson.Value -> Either String (ModLink doc)
fromJson fromJsonDoc = \case
  Aeson.Object obj -> do
    nameJson <- JsonHelpers.lookupField obj "name"
    n <- ModuleName.fromJson nameJson
    labelJson <- JsonHelpers.lookupField obj "label"
    lbl <- case labelJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (fromJsonDoc labelJson)
    Right $ MkModLink {name = n, label = lbl}
  _ -> Left "ModLink must be an object"

toJson :: (doc -> Aeson.Value) -> ModLink doc -> Aeson.Value
toJson toJsonDoc (MkModLink n lbl) =
  Aeson.object
    [ "name" Aeson..= ModuleName.toJson n,
      "label" Aeson..= maybe Aeson.Null toJsonDoc lbl
    ]
