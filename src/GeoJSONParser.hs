{-# LANGUAGE OverloadedStrings #-}

module GeoJSONParser ( parseFeatureCollection ) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Text

data GeoJSONFeatureCollection =
    GeoJSONFeatureCollection { fc_type :: !Text
                             , fc_features :: [GeoJSONFeature]
                             } deriving Show

instance FromJSON GeoJSONFeatureCollection where
    parseJSON = withObject "GeoJSONFeatureCollection" $ \obj -> do
        _type <- obj .: "type"
        features <- obj .: "features"
        return (GeoJSONFeatureCollection { fc_type = _type, fc_features = features })

data GeoJSONFeature =
    GeoJSONFeature { ft_type :: !Text
                   } deriving Show

instance FromJSON GeoJSONFeature where
    parseJSON = withObject "GeoJSONFeature" $ \obj -> do
        ft_type <- obj .: "type"
        return (GeoJSONFeature { ft_type = ft_type })

parseFeatureCollection :: B.ByteString -> Maybe GeoJSONFeatureCollection
parseFeatureCollection = decode
