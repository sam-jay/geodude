{-# LANGUAGE QuasiQuotes #-}

import GeoJSONParser (parseFeatureCollection, GeoJSONFeatureCollection)
import Data.Aeson
import Data.Aeson.QQ
import qualified Data.ByteString.Lazy as B
import Entities
import Geometry (containsP)
import RTree
import BoundingBox (Point)

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

loadStates = do
    x <- B.readFile "../data/states_provinces.json"
    case parseFeatureCollection x of
        Nothing -> error "error parsing feature collection"
        Just fc -> case parseStates fc of
                       Nothing -> error "error parsing states"
                       Just states -> putStrLn $ show states

loadCountries = do
    x <- B.readFile "../data/test.json"
    case parseFeatureCollection x of
        Just fcs -> case parseCountries fcs of
                        Just countries -> do
                            let c = cGeometry.head $ countries
                            -- let tree = insert newTree c
                            --let (Node _ children) =  fromList countries
                            let point = (-69.95441436767578, 12.518703864466934)
                            let ok = containsP c point
                            -- let ok = contains nt point
                            -- printTree "" tree
                            --mapM_ (putStrLn. show. depth) children
                            return ok

