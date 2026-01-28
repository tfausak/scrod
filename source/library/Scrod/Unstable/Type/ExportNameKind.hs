module Scrod.Unstable.Type.ExportNameKind where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json

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

toJson :: ExportNameKind -> Json.Json
toJson kind = case kind of
  Pattern -> Json.tag (Text.pack "Pattern")
  Type -> Json.tag (Text.pack "Type")
  Module -> Json.tag (Text.pack "Module")
