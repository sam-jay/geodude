{-# LANGUAGE QuasiQuotes #-}

import GeoJSONParser (parseFeatureCollection)
import Data.Aeson
import Data.Aeson.QQ
import qualified Data.ByteString.Lazy as B
import qualified Entities as E
import RTree
import BoundingBox
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
analyzeTree depth' x = do
    case x of
        Node _ cs -> do
            tell $ "Node with " ++ show numChildren ++
                   " children at depth " ++ show depth' ++ "\n"
            tell $ concatMap (snd . runWriter . analyzeTree (depth' + 1)) cs
            return $ numChildren
            where numChildren = length cs
        Leaf _ _ -> do
            tell ("Leaf at depth " ++ show depth' ++ "\n")
            return $ 1
        Empty -> do
            tell ("Empty at depth " ++ show depth' ++ "\n")
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

loadStates :: IO Int
loadStates = do
    x <- B.readFile "../data/full/states_provinces.json"
    case parseFeatureCollection x of
        Nothing -> error "error parsing feature collection"
        Just fc -> case E.parseStates fc of
                       Nothing -> error "error parsing states"
                       Just states -> do 
                          let tree = fromList states
                              -- point = (-69.95441436767578, 12.518703864466934)
                              -- ok = contains tree point
                              -- result = filter (\leaf -> E.containsPoint (getElem leaf) point) ok
                          return $ depth tree


loadCountries :: IO [RTree E.Entity]
loadCountries = do
    x <- B.readFile "../data/full/countries.json"
    case parseFeatureCollection x of
        Nothing -> error "no feature collection"
        Just fcs -> case E.parseCountries fcs of
                        Nothing -> error "no countries"
                        Just countries -> do
                            let c = countries
                            let tree = fromList c
                            --let (Node _ children) =  fromList countries
                            let point = (-69.95441436767578, 12.518703864466934)
                            let ok = contains tree point
                                result = filter (\leaf -> E.containsPoint (getElem leaf) point) ok
                            -- let ok = contains nt point
                            -- printTree "" tree
                            -- print $ depth tree
                            --mapM_ (putStrLn. show. depth) children
                            return result
