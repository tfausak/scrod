module Scrod
  ( -- * Main entry point
    Main.defaultMain,
    Main.extract,

    -- * Core types
    Interface.Interface,
    Doc.Doc,
    Export.Export,
    Item.Item,

    -- * JSON serialization
    JsonInterface.fromJson,
    JsonInterface.toJson,
    Json.parse,
    Json.render,

    -- * HTML rendering
    HtmlInterface.toHtml,
  )
where

import qualified Scrod.Html.Interface as HtmlInterface
import qualified Scrod.Json.Interface as JsonInterface
import qualified Scrod.Json.Utilities as Json
import qualified Scrod.Main as Main
import qualified Scrod.Type.Doc as Doc
import qualified Scrod.Type.Export as Export
import qualified Scrod.Type.Interface as Interface
import qualified Scrod.Type.Item as Item
