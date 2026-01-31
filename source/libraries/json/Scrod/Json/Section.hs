module Scrod.Json.Section where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Doc as Doc
import qualified Scrod.Json.Header as Header
import qualified Scrod.Type.Section as Type

fromJson :: Aeson.Value -> Either String Type.Section
fromJson v = fmap Type.MkSection (Header.fromJson Doc.fromJson v)

toJson :: Type.Section -> Aeson.Value
toJson (Type.MkSection hdr) = Header.toJson Doc.toJson hdr
