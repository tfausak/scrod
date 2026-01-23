module Scrod.Unstable.Type.Language where

import qualified Data.Text as Text
import qualified GHC.Driver.Flags as Flags

newtype Language = MkLanguage
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromString :: String -> Language
fromString =
  MkLanguage
    . Text.pack

fromGhc :: Flags.Language -> Language
fromGhc =
  fromString
    . show
