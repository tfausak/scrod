module Scrod.Unstable.Type.Picture where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json

-- | A picture/image reference.
-- Mirrors 'Documentation.Haddock.Types.Picture' from haddock-library,
-- but uses 'Text' instead of 'String'.
data Picture = MkPicture
  { uri :: Text.Text,
    title :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show)

toJson :: Picture -> Json.Json
toJson (MkPicture u t) =
  Json.object
    [ (Text.pack "uri", Json.fromText u),
      (Text.pack "title", maybe Json.Null Json.fromText t)
    ]
