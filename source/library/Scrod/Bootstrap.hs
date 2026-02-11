{-# LANGUAGE TemplateHaskell #-}

module Scrod.Bootstrap where

import qualified Data.Text as Text
import qualified Scrod.Extra.TemplateHaskell as TH

css :: Text.Text
css = Text.pack $$(TH.embedFile "extra/bootstrap/bootstrap.min.css")
