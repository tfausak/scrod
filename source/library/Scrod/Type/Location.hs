{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Type.Location where

import qualified Data.Aeson as Aeson
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Scrod.Type.Column as Column
import qualified Scrod.Type.JsonHelpers as JsonHelpers
import qualified Scrod.Type.Line as Line

data Location = MkLocation
  { line :: Line.Line,
    column :: Column.Column
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Location
fromJson = \case
  Aeson.Object obj -> do
    lineJson <- JsonHelpers.lookupField obj "line"
    l <- Line.fromJson lineJson
    columnJson <- JsonHelpers.lookupField obj "column"
    c <- Column.fromJson columnJson
    Right $ MkLocation {line = l, column = c}
  _ -> Left "Location must be an object"

toJson :: Location -> Aeson.Value
toJson (MkLocation l c) =
  Aeson.object
    [ "line" Aeson..= Line.toJson l,
      "column" Aeson..= Column.toJson c
    ]

fromSrcLoc :: SrcLoc.SrcLoc -> Maybe Location
fromSrcLoc srcLoc = case srcLoc of
  SrcLoc.RealSrcLoc realSrcLoc _ -> do
    line <- Line.fromInt $ SrcLoc.srcLocLine realSrcLoc
    column <- Column.fromInt $ SrcLoc.srcLocCol realSrcLoc
    pure
      MkLocation
        { line,
          column
        }
  SrcLoc.UnhelpfulLoc _ -> Nothing

fromSrcSpan :: SrcLoc.SrcSpan -> Maybe Location
fromSrcSpan = fromSrcLoc . SrcLoc.srcSpanStart
