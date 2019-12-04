{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Entities (
    Entity,
    parseStates,
    parseCountries
) where

import Geometry
import GeoJSONParser (
    GeoJSONFeatureCollection
  , GeoJSONFeatureCollection (fcFeatures)
  , GeoJSONFeature
  , GeoJSONFeature (GeoJSONFeature, ftType, ftProperties, ftGeometry))

import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.Types (Value, Value (String, Null, Object, Number))
import qualified Data.Text as T
import BoundingBox (Boundable, getBoundingBox)

data Entity =
    Country { cGeometry :: Geometry
            , cName :: String
            , cAdmin :: String }
  | State { sGeometry :: Geometry
          , sName :: Maybe String
          , sAdmin :: String } deriving (Eq)

instance Show Entity where
  show (Country {cName}) = "Country{ " ++ show cName ++ "}"
  show (State {sName}) = "State{ " ++ show sName ++ "}"  

instance Boundable Entity where
    getBoundingBox Country { cGeometry } = getBoundingBox cGeometry
    getBoundingBox State { sGeometry } = getBoundingBox sGeometry


parseCountries :: GeoJSONFeatureCollection -> Maybe [Entity]
parseCountries = mapM featureToCountry . fcFeatures

featureToCountry :: GeoJSONFeature -> Maybe Entity
featureToCountry GeoJSONFeature { ftType, ftProperties, ftGeometry } = do
    name <- extractText <$> Map.lookup "NAME" ftProperties
    admin <- extractText <$> Map.lookup "ADMIN" ftProperties
    return $ Country { cGeometry = ftGeometry
                     , cName = name
                     , cAdmin = admin }

parseStates :: GeoJSONFeatureCollection -> Maybe [Entity]
parseStates = mapM featureToState . fcFeatures

featureToState :: GeoJSONFeature -> Maybe Entity
featureToState GeoJSONFeature { ftType, ftProperties, ftGeometry } = do
    name <- extractMaybeText <$> Map.lookup "name" ftProperties
    admin <- extractText <$> Map.lookup "admin" ftProperties
    return $ State { sGeometry = ftGeometry, sName = name, sAdmin = admin }

extractText :: Value -> String
extractText (String t) = T.unpack t

extractMaybeText :: Value -> Maybe String
extractMaybeText (String t) = Just $ T.unpack t
extractMaybeText Null = Nothing
