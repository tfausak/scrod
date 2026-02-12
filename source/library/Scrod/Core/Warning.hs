{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Warning where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Core.Category as Category
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

data Warning = MkWarning
  { category :: Category.Category,
    value :: Text.Text
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically Warning instance ToJson.ToJson Warning

deriving via Generics.Generically Warning instance Schema.ToSchema Warning
