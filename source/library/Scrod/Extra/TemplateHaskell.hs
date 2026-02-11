module Scrod.Extra.TemplateHaskell where

import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Directory as Directory

embedFile :: FilePath -> TH.Code TH.Q String
embedFile relative = TH.liftCode $ do
  absolute <- TH.runIO $ Directory.makeAbsolute relative
  TH.addDependentFile absolute
  content <- TH.runIO $ readFile absolute
  TH.examineCode $ TH.liftTyped content
