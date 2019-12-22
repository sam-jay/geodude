{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-  References:

    GeoJSON specification: https://tools.ietf.org/html/rfc7946
-}

module GeoJSONParser (
    parseFeatureCollection,
    GeoJSONFeatureCollection (..),
    GeoJSONFeature (..)
) where

import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Aeson
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Geometry (Geometry)

parseFeatureCollection :: B.ByteString -> Maybe GeoJSONFeatureCollection
parseFeatureCollection = decode

data GeoJSONFeatureCollection =
    GeoJSONFeatureCollection { fcType :: String
                             , fcFeatures :: [GeoJSONFeature]
                             } deriving (Show, Generic)

instance FromJSON GeoJSONFeatureCollection where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = defaultFieldLabelModifier }

data GeoJSONFeature =
    GeoJSONFeature { ftType :: String
                   , ftProperties :: Map.Map String Value
                   , ftGeometry :: Geometry
                   } deriving (Show, Generic)

instance FromJSON GeoJSONFeature where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = defaultFieldLabelModifier }

defaultFieldLabelModifier :: String -> String
defaultFieldLabelModifier = map toLower . drop 2
