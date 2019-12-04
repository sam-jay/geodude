{-# LANGUAGE QuasiQuotes #-}

import GeoJSONParser (parseFeatureCollection, GeoJSONFeatureCollection)
import Data.Aeson
import Data.Aeson.QQ
import qualified Data.ByteString.Lazy as B
import Entities (parseStates, parseCountries, Entity)
import RTree
import BoundingBox (Boundable, Point, getBoundingBox)
import Test.QuickCheck

emptyFeatureCollection :: Value
emptyFeatureCollection = [aesonQQ|
  {
    "type": "FeatureCollection",
    "features": []
  }
|]

featureCollectionWithPolygon :: Value
featureCollectionWithPolygon = [aesonQQ|
  {
    "type": "FeatureCollection",
    "features": [
      {
        "type": "Feature",
        "properties": {},
        "geometry": {
            "type": "Polygon",
            "coordinates": [[[1.1, 1.2], [-1.1, 1.2], [-1.1, -1.2], [1.1, 1.2]]]
        }
      }
    ]
  }
|]


-- check that list -> rtree -> list is preserved
prop_identity :: [Entity] -> Bool
prop_identity xs = (toList . fromList) xs == xs

-- check that foldl1 (\bb a -> ) as == getBoundingBox . fromList as
-- prop_bbox xs = (getBoundingBox . fromList) xs == getBoundingBox $ combine xs


loadStates = do
    x <- B.readFile "../data/states_provinces.json"
    case parseFeatureCollection x of
        Nothing -> error "error parsing feature collection"
        Just fc -> case parseStates fc of
                       Nothing -> error "error parsing states"
                       Just states -> putStrLn $ show states

loadCountries = do
    x <- B.readFile "../data/countries.json"
    case parseFeatureCollection x of
        Just fcs -> case parseCountries fcs of
                        Just countries -> do
                            let c = head $ countries
                            let nt =  insert newTree c
                            return nt

