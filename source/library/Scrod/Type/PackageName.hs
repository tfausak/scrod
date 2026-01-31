module Scrod.Type.PackageName where

import qualified Data.Text as Text

newtype PackageName = MkPackageName
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromString :: String -> PackageName
fromString =
  MkPackageName
    . Text.pack
