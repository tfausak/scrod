module Scrod.Unstable.Type.JsonOptions
  ( sumOptions,
  )
where

import qualified Data.Aeson as Aeson

sumOptions :: Aeson.Options
sumOptions =
  Aeson.defaultOptions
    { Aeson.sumEncoding = Aeson.TaggedObject "tag" "contents",
      Aeson.allNullaryToStringTag = False
    }
