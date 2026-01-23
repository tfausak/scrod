module Scrod.Unstable.Type.Extension where

import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified GHC.Driver.Session as Session
import qualified GHC.LanguageExtensions.Type as Ghc

newtype Extension = MkExtension
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromString :: String -> Extension
fromString =
  MkExtension
    . Text.pack

fromGhc :: Ghc.Extension -> Extension
fromGhc x =
  fromString $ Map.findWithDefault (show x) x extensionNameExceptions

-- | Map from GHC Extension to its preferred string representation,
-- containing only extensions where the canonical name differs from 'show'.
extensionNameExceptions :: Map.Map Ghc.Extension String
extensionNameExceptions =
  Identity.runIdentity
    . Map.traverseMaybeWithKey
      ( \extension names -> do
          let shown = show extension
          pure
            . maybe (Maybe.listToMaybe names) (const Nothing)
            $ List.find (== shown) names
      )
    . Map.fromListWith (<>)
    $ fmap (\x -> (Session.flagSpecFlag x, [Session.flagSpecName x])) Session.xFlags
