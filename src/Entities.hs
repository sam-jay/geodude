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
import GeoJSONParser ( GeoJSONFeatureCollection(..)
                     , GeoJSONFeature(..)
                     )
import qualified Data.Map.Strict as Map
import Data.Aeson.Types (Value, Value (String))
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
    e1 `compare` e2 = a1 `compare` a2
        where a1 = area $ getBoundingBox e1
              a2 = area $ getBoundingBox e2

instance Show Entity where
  show Country { cName } = "Country{ " ++ show cName ++ " }"
  show State { sName } = "State{ " ++ show sName ++ " }"

instance Boundable Entity where
    getBoundingBox Country { cGeometry } = getBoundingBox cGeometry
    getBoundingBox State { sGeometry } = getBoundingBox sGeometry

parseCountries :: GeoJSONFeatureCollection -> Maybe [Entity]
parseCountries = mapM featureToCountry . fcFeatures

featureToCountry :: GeoJSONFeature -> Maybe Entity
featureToCountry GeoJSONFeature { ftProperties, ftGeometry } = do
    name <- extractText <$> Map.lookup "NAME" ftProperties
    admin <- extractText <$> Map.lookup "ADMIN" ftProperties
    return $ Country { cGeometry = ftGeometry
                     , cName = name
                     , cAdmin = admin
                     }

parseStates :: GeoJSONFeatureCollection -> Maybe [Entity]
parseStates = mapM featureToState . fcFeatures

featureToState :: GeoJSONFeature -> Maybe Entity
featureToState GeoJSONFeature { ftProperties, ftGeometry } = do
    name <- extractMaybeText <$> Map.lookup "name" ftProperties
    admin <- extractText <$> Map.lookup "admin" ftProperties
    return $ State { sGeometry = ftGeometry
                   , sName = name
                   , sAdmin = admin
                   }

extractText :: Value -> String
extractText (String t) = T.unpack t
extractText _ = error "not text"

extractMaybeText :: Value -> Maybe String
extractMaybeText (String t) = Just $ T.unpack t
extractMaybeText _ = Nothing

containsPoint :: Entity -> (Double, Double) -> Bool
containsPoint (Country {cGeometry}) p = containsP p cGeometry
containsPoint (State {sGeometry}) p = containsP p sGeometry

buildEntityWithGeo :: Geometry -> Entity
buildEntityWithGeo geo = State { sGeometry = geo
                               , sName = Nothing
                               , sAdmin = "NA" }
