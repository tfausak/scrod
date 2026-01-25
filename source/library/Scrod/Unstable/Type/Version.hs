module Scrod.Unstable.Type.Version where

import Control.Monad ((<=<))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Version as Version
import qualified Documentation.Haddock.Types as Haddock
import qualified Numeric.Natural as Natural
import qualified Scrod.Unstable.Extra.Natural as Natural

newtype Version = MkVersion
  { value :: NonEmpty.NonEmpty Natural.Natural
  }
  deriving (Eq, Ord, Show)

fromHaddock :: Haddock.Version -> Maybe Version
fromHaddock =
  fmap MkVersion
    . NonEmpty.nonEmpty
    <=< traverse Natural.fromInt

fromBase :: Version.Version -> Maybe Version
fromBase =
  fromHaddock
    . Version.versionBranch
