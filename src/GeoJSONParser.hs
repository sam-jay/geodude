{-# LANGUAGE OverloadedStrings #-}

module GeoJSONParser ( parseCountry, parseState ) where

import qualified Data.ByteString.Lazy as B
import Control.Monad
import Data.Aeson
import Data.Text
import Control.Applicative

-- data GeoJSONFeatureCollection =
--     GeoJSONFeatureCollection { fc_type :: !Text
--                              , fc_features :: [GeoJSONFeature]
--                              } deriving Show

-- instance FromJSON GeoJSONFeatureCollection where
--     parseJSON = withObject "GeoJSONFeatureCollection" $ \obj -> do
--         _type <- obj .: "type"
--         features <- obj .: "features"
--         return (GeoJSONFeatureCollection { fc_type = _type, fc_features = features })

data Geometry = Polygon [[[Double]]] 
                | MultiPolygon [[[[Double]]]]
                deriving (Show)

instance FromJSON Geometry where
 parseJSON v = (Polygon <$> parseJSON v)
                 <|> (MultiPolygon <$> parseJSON v)

data CountryGeo =
    CountryGeo { cid :: Int,
                 countryName :: !Text,
                  cGeometry :: Geometry
                  --other fields
               } deriving (Show)

data StateGeo = 
    StateGeo { sid :: Int,
               stateName :: !Text,
               sGeometry :: Geometry
               -- other fields
            } deriving (Show)
    
instance FromJSON CountryGeo where
    parseJSON = withObject "CountryGeo" $ \obj -> do
        ft_id <- obj .: "id"
        ft_prop <- obj .: "properties"
        ft_name <- ft_prop .: "SOVEREIGNT"
        ft_geo <- obj .: "geometry"
        ft_coo <- ft_geo .: "coordinates"
        ft_geometry <- parseJSON ft_coo
        return (CountryGeo { cid = ft_id, countryName = ft_name, cGeometry = ft_geometry})

instance FromJSON StateGeo where
    parseJSON = withObject "StateGeo" $ \obj -> do
        ft_prop <- obj .: "properties"
        ft_name <- ft_prop .: "name"
        ft_id <- ft_prop .: "ne_id"
        ft_geo <- obj .: "geometry"
        ft_coo <- ft_geo .: "coordinates"
        ft_geometry <- parseJSON ft_coo
        return (StateGeo { sid = ft_id, stateName = ft_name, sGeometry = ft_geometry})

parseCountry :: B.ByteString -> Maybe [CountryGeo]
parseCountry = decode

parseState :: B.ByteString -> Maybe [StateGeo]
parseState = decode