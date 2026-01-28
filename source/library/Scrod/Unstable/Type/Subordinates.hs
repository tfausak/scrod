module Scrod.Unstable.Type.Subordinates where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.ExportName as ExportName
import qualified Scrod.Unstable.Type.Json as Json

-- | Subordinate exports for a type or class.
-- Represents the contents of parentheses in exports like @Foo(..)@ or @Foo(Bar, Baz)@.
data Subordinates = MkSubordinates
  { -- | Whether a @(..)@ wildcard is present.
    wildcard :: Bool,
    -- | Explicitly listed children.
    explicit :: [ExportName.ExportName]
  }
  deriving (Eq, Ord, Show)

toJson :: Subordinates -> Json.Json
toJson (MkSubordinates w e) =
  Json.object
    [ (Text.pack "wildcard", Json.fromBool w),
      (Text.pack "explicit", Json.fromList $ fmap ExportName.toJson e)
    ]
