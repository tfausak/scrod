{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Picture where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | A picture/image reference.
data Picture = MkPicture
  { uri :: Text.Text,
    title :: Maybe Text.Text
  }
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically Picture
