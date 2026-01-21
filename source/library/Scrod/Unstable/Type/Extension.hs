module Scrod.Unstable.Type.Extension where

import qualified Data.Text as Text
import qualified GHC.LanguageExtensions.Type as Extension

newtype Extension = MkExtension
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromString :: String -> Extension
fromString =
  MkExtension
    . Text.pack

fromGhc :: Extension.Extension -> Extension
fromGhc =
  fromString
    -- TODO: This isn't exactly right because capitalization can differ. For
    -- example the constructor is called `Cpp` but the actual extension name is
    -- `CPP`. I think it's safe to say that extensions will never differ only
    -- by case, so perhaps these should be normalized to lowercase.
    . show
