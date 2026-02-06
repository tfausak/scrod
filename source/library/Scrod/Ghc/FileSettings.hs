{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Ghc.FileSettings where

import qualified GHC.Driver.Session as Session
import qualified GHC.Stack as Stack
import qualified Scrod.Ghc.Uninitialized as Uninitialized

empty :: (Stack.HasCallStack) => Session.FileSettings
empty =
  Session.FileSettings
    { Session.fileSettings_ghcUsagePath = Uninitialized.throw 'Session.fileSettings_ghcUsagePath,
      Session.fileSettings_ghciUsagePath = Uninitialized.throw 'Session.fileSettings_ghciUsagePath,
      Session.fileSettings_globalPackageDatabase = Uninitialized.throw 'Session.fileSettings_globalPackageDatabase,
      Session.fileSettings_toolDir = Uninitialized.throw 'Session.fileSettings_toolDir,
      Session.fileSettings_topDir = Uninitialized.throw 'Session.fileSettings_topDir
    }
