{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.Picture where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics

-- | A picture/image reference.
data Picture = MkPicture
  { uri :: Text.Text,
    title :: Maybe Text.Text
  }
  deriving (Eq, Generics.Generic, Ord, Show)
