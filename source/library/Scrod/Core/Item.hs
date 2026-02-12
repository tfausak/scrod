{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Item where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.ItemName as ItemName
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

data Item = MkItem
  { key :: ItemKey.ItemKey,
    kind :: ItemKind.ItemKind,
    parentKey :: Maybe ItemKey.ItemKey,
    name :: Maybe ItemName.ItemName,
    documentation :: Doc.Doc,
    signature :: Maybe Text.Text
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically Item instance ToJson Item

deriving via Generics.Generically Item instance ToSchema Item
