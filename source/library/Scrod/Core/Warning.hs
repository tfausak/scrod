{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Warning where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Core.Category as Category
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

data Warning = MkWarning
  { category :: Category.Category,
    value :: Text.Text
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically Warning instance ToJson Warning

deriving via Generics.Generically Warning instance ToSchema Warning
