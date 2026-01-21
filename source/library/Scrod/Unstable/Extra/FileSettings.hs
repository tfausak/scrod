{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Unstable.Extra.FileSettings where

import qualified GHC.Driver.Session as Session
import qualified Scrod.Unstable.Exception.Uninitialized as Uninitialized

empty :: Session.FileSettings
empty =
  Session.FileSettings
    { Session.fileSettings_ghcUsagePath = Uninitialized.throw 'Session.fileSettings_ghcUsagePath,
      Session.fileSettings_ghciUsagePath = Uninitialized.throw 'Session.fileSettings_ghciUsagePath,
      Session.fileSettings_globalPackageDatabase = Uninitialized.throw 'Session.fileSettings_globalPackageDatabase,
      Session.fileSettings_toolDir = Uninitialized.throw 'Session.fileSettings_toolDir,
      Session.fileSettings_topDir = Uninitialized.throw 'Session.fileSettings_topDir
    }
