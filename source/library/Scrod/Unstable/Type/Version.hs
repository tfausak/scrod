{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrod.Unstable.Type.Version where

import Control.Monad ((<=<))
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector
import qualified Data.Version as Version
import qualified Documentation.Haddock.Types as Haddock
import qualified Numeric.Natural as Natural
import qualified Scrod.Unstable.Extra.Natural as Natural

newtype Version = MkVersion
  { value :: NonEmpty.NonEmpty Natural.Natural
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Version
fromJson = \case
  Aeson.Array vec -> do
    naturals <- traverse fromJsonNatural (Vector.toList vec)
    case NonEmpty.nonEmpty naturals of
      Nothing -> Left "Version must be a non-empty list"
      Just ne -> Right (MkVersion ne)
  _ -> Left "Version must be an array"
  where
    fromJsonNatural :: Aeson.Value -> Either String Natural.Natural
    fromJsonNatural = \case
      Aeson.Number n -> case Scientific.floatingOrInteger n of
        Right i | i >= 0 -> Right (fromInteger i)
        Right i -> Left $ "Version components must be non-negative, got: " <> show i
        Left (_ :: Double) -> Left "Version components must be integers"
      _ -> Left "Version components must be numbers"

toJson :: Version -> Aeson.Value
toJson (MkVersion ns) =
  Aeson.Array . Vector.fromList $
    fmap (\n -> Aeson.Number (Scientific.scientific (fromIntegral n) 0)) (NonEmpty.toList ns)

fromHaddock :: Haddock.Version -> Maybe Version
fromHaddock =
  fmap MkVersion
    . NonEmpty.nonEmpty
    <=< traverse Natural.fromInt

fromBase :: Version.Version -> Maybe Version
fromBase =
  fromHaddock
    . Version.versionBranch
