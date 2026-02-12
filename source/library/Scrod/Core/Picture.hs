{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

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

deriving via Generics.Generically Picture instance ToJson.ToJson Picture

deriving via Generics.Generically Picture instance Schema.ToSchema Picture
