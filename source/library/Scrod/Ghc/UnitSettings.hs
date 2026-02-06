module Scrod.Ghc.UnitSettings where

import qualified GHC.Settings as Settings
import qualified Scrod.Ghc.UnitId as UnitId

empty :: Settings.UnitSettings
empty = Settings.UnitSettings {Settings.unitSettings_baseUnitId = UnitId.empty}
