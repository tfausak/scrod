{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Header where

import qualified Data.Aeson as Aeson
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers
import qualified Scrod.Unstable.Type.Level as Level

-- | A section header with a level and title.
-- Mirrors 'Documentation.Haddock.Types.Header' from haddock-library.
data Header doc = MkHeader
  { level :: Level.Level,
    title :: doc
  }
  deriving (Eq, Ord, Show)

fromJson :: (Aeson.Value -> Either String doc) -> Aeson.Value -> Either String (Header doc)
fromJson fromJsonDoc = \case
  Aeson.Object obj -> do
    lvlJson <- JsonHelpers.lookupField obj "level"
    lvl <- Level.fromJson lvlJson
    titleJson <- JsonHelpers.lookupField obj "title"
    t <- fromJsonDoc titleJson
    Right $ MkHeader {level = lvl, title = t}
  _ -> Left "Header must be an object"

toJson :: (doc -> Aeson.Value) -> Header doc -> Aeson.Value
toJson toJsonDoc (MkHeader lvl t) =
  Aeson.object
    [ "level" Aeson..= Level.toJson lvl,
      "title" Aeson..= toJsonDoc t
    ]
