{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GeoJSONParser ( parseFeatureCollection ) where

import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import Data.Char (toUpper, toLower)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Bifunctor

data GeoJSONFeatureCollection =
    GeoJSONFeatureCollection { fcType :: String
                             , fcFeatures :: [GeoJSONFeature]
                             } deriving (Show, Generic)

instance FromJSON GeoJSONFeatureCollection where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 2 }

data GeoJSONFeature =
    GeoJSONFeature { ftType :: String
                   , ftProperties :: Map.Map String Value
                   , ftGeometry :: GeoJSONGeometry
                   } deriving (Show, Generic)

instance FromJSON GeoJSONFeature where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 2 }

data GeoJSONGeometry =
    GeoJSONPolygon { plType :: String
                   , plCoordinates :: [[(Float, Float)]]
                   }
  | GeoJSONMultiPolygon { mpType :: String
                        , mpCoordinates :: [[[(Float, Float)]]]
                        } deriving Show

instance FromJSON GeoJSONGeometry where
    parseJSON = withObject "GeoJSONGeometry" $ \obj -> do
        _type <- obj .: "type"
        case _type of
            "Polygon" -> do coordinates <- obj .: "coordinates"
                            return (GeoJSONPolygon { plType = _type
                                                   , plCoordinates = coordinates })
            "MultiPolygon" -> do coordinates <- obj .: "coordinates"
                                 return (GeoJSONMultiPolygon { mpType = _type
                                                             , mpCoordinates = coordinates })
            _ -> error "unknown geometry"

parseFeatureCollection :: B.ByteString -> Maybe GeoJSONFeatureCollection
parseFeatureCollection = decode

