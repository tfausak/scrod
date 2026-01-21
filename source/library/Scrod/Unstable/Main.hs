module Scrod.Unstable.Main where

import qualified Scrod.Unstable.Convert as Convert
import qualified Scrod.Unstable.Parse as Parse
import qualified Scrod.Unstable.Type.Interface as Interface

defaultMain :: IO ()
defaultMain = do
  contents <- getContents
  print $ extract contents

extract :: String -> Either String Interface.Interface
extract = Convert.convert . Parse.parse
