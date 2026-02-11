{-# LANGUAGE TemplateHaskell #-}

module Scrod.Bootstrap where

import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Directory as Directory

css :: Text.Text
css =
  Text.pack
    $( do
         let filePath = "extra/bootstrap/bootstrap.min.css"
         absolutePath <- TH.runIO $ Directory.makeAbsolute filePath
         TH.addDependentFile absolutePath
         content <- TH.runIO $ readFile filePath
         TH.lift content
     )
