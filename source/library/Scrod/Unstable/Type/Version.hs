module Scrod.Unstable.Type.Version where

import Control.Monad ((<=<))
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Version as Version
import qualified Documentation.Haddock.Types as Haddock
import qualified Numeric.Natural as Natural
import qualified Scrod.Unstable.Extra.Natural as Natural

newtype Version = MkVersion
  { value :: NonEmpty.NonEmpty Natural.Natural
  }
  deriving (Eq, Ord, Show)

instance Aeson.FromJSON Version where
  parseJSON v = do
    ns <- Aeson.parseJSON v
    case NonEmpty.nonEmpty ns of
      Nothing -> fail "Version must be a non-empty list"
      Just ne -> pure $ MkVersion ne

instance Aeson.ToJSON Version where
  toJSON (MkVersion ns) = Aeson.toJSON $ NonEmpty.toList ns

fromHaddock :: Haddock.Version -> Maybe Version
fromHaddock =
  fmap MkVersion
    . NonEmpty.nonEmpty
    <=< traverse Natural.fromInt

fromBase :: Version.Version -> Maybe Version
fromBase =
  fromHaddock
    . Version.versionBranch
