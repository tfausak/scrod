module Scrod.Unstable.Type.Hyperlink where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json

-- | A hyperlink with an optional label.
-- Mirrors 'Documentation.Haddock.Types.Hyperlink' from haddock-library,
-- but uses 'Text' instead of 'String' for the URL.
data Hyperlink doc = MkHyperlink
  { url :: Text.Text,
    label :: Maybe doc
  }
  deriving (Eq, Ord, Show)

toJson :: (doc -> Json.Json) -> Hyperlink doc -> Json.Json
toJson f (MkHyperlink u l) =
  Json.object
    [ (Text.pack "url", Json.fromText u),
      (Text.pack "label", maybe Json.Null f l)
    ]
