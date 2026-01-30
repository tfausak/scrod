{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Picture where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers

-- | A picture/image reference.
-- Mirrors 'Documentation.Haddock.Types.Picture' from haddock-library,
-- but uses 'Text' instead of 'String'.
data Picture = MkPicture
  { uri :: Text.Text,
    title :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Picture
fromJson = \case
  Aeson.Object obj -> do
    uriJson <- JsonHelpers.lookupField obj "uri"
    u <- case uriJson of
      Aeson.String t -> Right t
      _ -> Left "uri must be a string"
    titleJson <- JsonHelpers.lookupField obj "title"
    t <- case titleJson of
      Aeson.Null -> Right Nothing
      Aeson.String txt -> Right (Just txt)
      _ -> Left "title must be a string or null"
    Right $ MkPicture {uri = u, title = t}
  _ -> Left "Picture must be an object"

toJson :: Picture -> Aeson.Value
toJson (MkPicture u t) =
  Aeson.object
    [ "uri" Aeson..= u,
      "title" Aeson..= maybe Aeson.Null Aeson.String t
    ]
