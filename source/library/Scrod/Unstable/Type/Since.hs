module Scrod.Unstable.Type.Since where

import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.Json as Json
import qualified Scrod.Unstable.Type.PackageName as PackageName
import qualified Scrod.Unstable.Type.Version as Version

data Since = MkSince
  { package :: Maybe PackageName.PackageName,
    version :: Version.Version
  }
  deriving (Eq, Ord, Show)

toJson :: Since -> Json.Json
toJson (MkSince p v) =
  Json.object
    [ (Text.pack "package", maybe Json.Null PackageName.toJson p),
      (Text.pack "version", Version.toJson v)
    ]
