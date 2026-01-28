module Scrod.Unstable.Type.Namespace where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json

-- | The namespace qualification for an identifier.
-- Mirrors 'Documentation.Haddock.Types.Namespace' from haddock-library,
-- but without the 'None' case (represented as 'Nothing' in 'Identifier').
data Namespace
  = -- | v'identifier' syntax
    Value
  | -- | t'identifier' syntax
    Type
  deriving (Eq, Ord, Show)

toJson :: Namespace -> Json.Json
toJson ns = case ns of
  Value -> Json.tag (Text.pack "Value")
  Type -> Json.tag (Text.pack "Type")
