module Scrod.Extra.TemplateHaskell where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Directory as Directory

embedFile :: FilePath -> TH.Q TH.Exp
embedFile filePath = do
  absolutePath <- TH.runIO $ Directory.makeAbsolute filePath
  TH.addDependentFile absolutePath
  content <- TH.runIO $ readFile filePath
  TH.lift content
