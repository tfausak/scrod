module Scrod.Extra.FileSettings where

import qualified GHC.Driver.Session as Session

emptyFileSettings :: Session.FileSettings
emptyFileSettings =
  Session.FileSettings
    { Session.fileSettings_ghciUsagePath = error "FileSettings.ghciUsagePath",
      Session.fileSettings_ghcUsagePath = error "FileSettings.ghcUsagePath",
      Session.fileSettings_globalPackageDatabase = error "FileSettings.globalPackageDatabase",
      Session.fileSettings_toolDir = error "FileSettings.toolDir",
      Session.fileSettings_topDir = error "FileSettings.topDir"
    }
