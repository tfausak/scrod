module Scrod.Json.Version where

import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vector
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.Version as Type

fromJson :: Aeson.Value -> Either String Type.Version
fromJson value = case value of
  Aeson.Array vec -> do
    naturals <- traverse Helpers.fromJsonNatural (Vector.toList vec)
    case NonEmpty.nonEmpty naturals of
      Nothing -> Left "Version must be a non-empty list"
      Just ne -> Right (Type.MkVersion ne)
  _ -> Left "Version must be an array"

toJson :: Type.Version -> Aeson.Value
toJson (Type.MkVersion ns) =
  Aeson.Array . Vector.fromList $
    fmap Aeson.toJSON (NonEmpty.toList ns)
