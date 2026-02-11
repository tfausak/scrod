{-# LANGUAGE TemplateHaskell #-}

module Scrod.Bootstrap where

import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

css :: Text.Text
css =
  Text.pack
    $( do
         let filePath = "extra/bootstrap/bootstrap.min.css"
         TH.addDependentFile filePath
         content <- TH.runIO $ readFile filePath
         TH.lift content
     )
