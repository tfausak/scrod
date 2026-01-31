module Scrod.Html.Utilities
  ( -- * Rendering
    render,
  )
where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Lucid

-- | Render HTML to a lazy ByteString.
render :: Lucid.Html () -> LazyByteString.ByteString
render = Lucid.renderBS
