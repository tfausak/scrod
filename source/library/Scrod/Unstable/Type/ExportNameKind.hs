{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.ExportNameKind where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers

-- | Namespace annotation for a name in an export list.
-- Corresponds to IEPattern and IEType from GHC's IEWrappedName.
-- Plain names (IEName) are represented as 'Nothing' in ExportName.
data ExportNameKind
  = -- | @pattern X@
    Pattern
  | -- | @type (:+:)@
    Type
  | -- | @module Data.List@
    Module
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String ExportNameKind
fromJson = \case
  Aeson.Object obj -> do
    tagJson <- JsonHelpers.lookupField obj "tag"
    tag <- case tagJson of
      Aeson.String t -> Right t
      _ -> Left "tag must be a string"
    case tag of
      "Pattern" -> Right Pattern
      "Type" -> Right Type
      "Module" -> Right Module
      _ -> Left $ "unknown ExportNameKind tag: " <> Text.unpack tag
  _ -> Left "ExportNameKind must be an object"

toJson :: ExportNameKind -> Aeson.Value
toJson kind = Aeson.object ["tag" Aeson..= tag]
  where
    tag :: Text.Text
    tag = case kind of
      Pattern -> "Pattern"
      Type -> "Type"
      Module -> "Module"
