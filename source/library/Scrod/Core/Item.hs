{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Item where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

data Item = MkItem
  { key :: ItemKey.ItemKey,
    kind :: ItemKind.ItemKind,
    parentKey :: Maybe ItemKey.ItemKey,
    name :: Maybe ItemName.ItemName,
    documentation :: Doc.Doc,
    signature :: Maybe Text.Text
  }
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically Item
