{-# LANGUAGE QuasiQuotes #-}

import GeoJSONParser (parseFeatureCollection, GeoJSONFeatureCollection)
import Data.Aeson
import Data.Aeson.QQ
import qualified Data.ByteString.Lazy as B
import qualified Entities as E
import RTree
import BoundingBox
import Test.QuickCheck
import qualified Data.Set as Set
import Data.List (sortBy, concatMap)
import Generator (genPolygons)
import Geometry (Geometry)
import Control.Monad.Writer

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


analyzeTree :: Int -> RTree a -> Writer String Int
analyzeTree depth x = do
    case x of
        Node bb cs -> do
            tell $ "Node with " ++ show numChildren ++
                   " children at depth " ++ show depth ++ "\n"
            tell $ concatMap (snd . runWriter . analyzeTree (depth + 1)) cs
            return $ numChildren
            where numChildren = length cs
        Leaf bb x -> do
            tell ("Leaf at depth " ++ show depth ++ "\n")
            return $ 1
        Empty -> do
            tell ("Empty at depth " ++ show depth ++ "\n")
            return $ 0


{-analyzeConstruction :: Boundable a => [a] -> IO ()
analyzeConstruction l = putStrLn . snd . runWriter $ helper newTree l
    where helper :: Boundable a => RTree a -> [a] -> Writer String Int
          helper tree elements = do

            let tree' = insert tree x
            tell $ "Tree after adding element:\n"
            tell $ snd $ runWriter $ analyzeTree 0 tree'
            helper tree' xs
-}

analyzeBalancing :: IO ()
analyzeBalancing = do
    putStrLn $ snd $ runWriter $ analyzeTree 0 testTree

testTree :: RTree Geometry
testTree = fromList testPolygons

testPolygons :: [Geometry]
testPolygons = concatMap (genPolygons 10) quadrants
    where quadrants = [ BoundingBox { x1 = 0, x2 = 0.49, y1 = 0, y2 = 0.49 }
                      , BoundingBox { x1 = 0.5, x2 = 1, y1 = 0, y2 = 0.49 }
                      , BoundingBox { x1 = 0, x2 = 0.49, y1 = 0.5, y2 = 1 }
                      , BoundingBox { x1 = 0.5, x2 = 1, y1 = 0.5, y2 = 1 }
                      ]


-- check that list -> rtree -> list is preserved
prop_identity :: [E.Entity] -> Bool
prop_identity xs = (sortBy compare . toList . fromList) xs == sortBy compare xs

-- check that foldl1 (\bb a -> ) as == getBoundingBox . fromList as
-- prop_bbox xs = (getBoundingBox . fromList) xs == getBoundingBox $ combine xs


loadStates = do
    x <- B.readFile "../data/states_provinces.json"
    case parseFeatureCollection x of
        Nothing -> error "error parsing feature collection"
        Just fc -> case E.parseStates fc of
                       Nothing -> error "error parsing states"
                       Just states -> putStrLn $ show states

loadCountries = do
    x <- B.readFile "../data/countries.json"
    case parseFeatureCollection x of
        Just fcs -> case E.parseCountries fcs of
                        Just countries -> do
                            let c = head $ countries
                            -- let tree = insert newTree c
                            --let (Node _ children) =  fromList countries
                            let point = (-69.95441436767578, 12.518703864466934)
                            let ok = E.containsPoint c point
                            -- let ok = contains nt point
                            -- printTree "" tree
                            --mapM_ (putStrLn. show. depth) children
                            return ok

