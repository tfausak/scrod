module Scrod.Unstable.Type.Html
  ( -- * Re-exports from Lucid
    Html,
    renderBS,

    -- * Rendering
    render,
  )
where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Lucid

-- | HTML content type, re-exported from Lucid.
type Html = Lucid.Html ()

-- | Render HTML to a lazy ByteString.
render :: Html -> LazyByteString.ByteString
render = Lucid.renderBS
