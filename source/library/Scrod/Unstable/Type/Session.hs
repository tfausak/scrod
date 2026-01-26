module Scrod.Unstable.Type.Session where

import qualified Scrod.Unstable.Type.Header as Header

-- | A session header wrapping a Header type.
newtype Session doc = MkSession
  { header :: Header.Header doc
  }
  deriving (Eq, Ord, Show)
