{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Entities (
    Entity,
    parseStates,
    parseCountries,
    containsPoint,
    buildEntityWithGeo
) where

import Geometry
import GeoJSONParser (
    GeoJSONFeatureCollection
  , GeoJSONFeatureCollection (fcFeatures)
  , GeoJSONFeature
  , GeoJSONFeature (GeoJSONFeature, ftType, ftProperties, ftGeometry))

import Test.QuickCheck (Arbitrary(arbitrary), elements, choose, Gen)
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.Types (Value, Value (String, Null, Object, Number))
import qualified Data.Text as T
import BoundingBox (area, Boundable, getBoundingBox)
import GHC.Generics (Generic)
import Control.DeepSeq

data Entity =
    Country { cGeometry :: Geometry
            , cName :: String
            , cAdmin :: String }
  | State { sGeometry :: Geometry
          , sName :: Maybe String
          , sAdmin :: String } deriving (Eq, Generic)


instance NFData Entity

instance Ord Entity where
    e1@(Country {}) `compare` e2@(State {}) = GT
    e1 `compare` e2 = (area $ getBoundingBox e1) `compare` (area $ getBoundingBox e2)

instance Arbitrary Entity where
    arbitrary = do
        b :: Int <- choose (1, 2)
        ls <- arbitrary :: Gen [(Double, Double)]
        name <- arbitrary
        admin <- arbitrary
        let poly = Polygon { pOuterRing = LinearRing { getLineString = ls }
                           , pInnerRings = []
                           }
        case b of
           1 -> return $ Country { cGeometry = poly
                                 , cName = name
                                 , cAdmin = admin
                                 }
           _ -> return $ State { sGeometry = poly
                               , sName = Just name
                               , sAdmin = admin
                               }

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

containsPoint :: Entity -> (Double, Double) -> Bool
containsPoint (Country {cGeometry}) p = containsP cGeometry p
containsPoint (State {sGeometry}) p = containsP sGeometry p

buildEntityWithGeo :: Geometry -> Entity
buildEntityWithGeo geo = State {sGeometry = geo, sName = Nothing, sAdmin = "NA"}

