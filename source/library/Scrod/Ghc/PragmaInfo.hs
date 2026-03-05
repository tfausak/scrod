module Scrod.Ghc.PragmaInfo where

import qualified GHC.Driver.DynFlags as DynFlags
import qualified GHC.Driver.Session as Session
import qualified GHC.LanguageExtensions.Type as Extension
import qualified GHC.Types.SafeHaskell as SafeHaskell

data PragmaInfo = MkPragmaInfo
  { language :: Maybe Session.Language,
    extensions :: [DynFlags.OnOff Extension.Extension],
    safeHaskell :: SafeHaskell.SafeHaskellMode
  }
  deriving (Eq, Show)
