{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Ghc.GhcNameVersion where

import qualified GHC.Driver.Session as Session
import qualified GHC.Stack as Stack
import qualified Scrod.Ghc.Uninitialized as Uninitialized

empty :: (Stack.HasCallStack) => Session.GhcNameVersion
empty =
  Session.GhcNameVersion
    { Session.ghcNameVersion_programName = Uninitialized.throw 'Session.ghcNameVersion_programName,
      Session.ghcNameVersion_projectVersion = Uninitialized.throw 'Session.ghcNameVersion_projectVersion
    }
