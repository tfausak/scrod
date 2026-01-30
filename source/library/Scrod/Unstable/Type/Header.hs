{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Header where

import qualified Scrod.Unstable.Type.Json as Json
import qualified Scrod.Unstable.Type.Level as Level

-- | A section header with a level and title.
-- Mirrors 'Documentation.Haddock.Types.Header' from haddock-library.
data Header doc = MkHeader
  { level :: Level.Level,
    title :: doc
  }
  deriving (Eq, Ord, Show)

toJson :: (doc -> Json.Json) -> Header doc -> Json.Json
toJson f (MkHeader l t) =
  Json.object
    [ ("level", Level.toJson l),
      ("title", f t)
    ]
