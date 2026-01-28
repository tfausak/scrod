module Scrod.Unstable.Type.Example where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json

-- | An example expression with its expected result.
-- Mirrors 'Documentation.Haddock.Types.Example' from haddock-library,
-- but uses 'Text' instead of 'String'.
data Example = MkExample
  { expression :: Text.Text,
    result :: [Text.Text]
  }
  deriving (Eq, Ord, Show)

toJson :: Example -> Json.Json
toJson (MkExample e r) =
  Json.object
    [ (Text.pack "expression", Json.fromText e),
      (Text.pack "result", Json.fromList $ fmap Json.fromText r)
    ]
