module Scrod.Unstable.Type.ExportNameKind where

-- | Namespace annotation for a name in an export list.
-- Corresponds to IEPattern and IEType from GHC's IEWrappedName.
-- Plain names (IEName) are represented as 'Nothing' in ExportName.
data ExportNameKind
  = -- | @pattern X@
    Pattern
  | -- | @type (:+:)@
    Type
  deriving (Eq, Ord, Show)
