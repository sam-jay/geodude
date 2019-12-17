module Evaluate where

import qualified Entities as E
import Geometry (Geometry, Point)
import GeoJSONParser (parseFeatureCollection, GeoJSONFeatureCollection)
import qualified RTree as RT
import Control.Parallel.Strategies (using, parList, rdeepseq)
import qualified Generator as G
import qualified Data.ByteString.Lazy as B
import Control.DeepSeq
import Data.List.Split (chunksOf)
import BoundingBox (BoundingBox(..), Boundable(..))
import System.Directory
import Control.Concurrent.ParallelIO.Local

data Execution = Parallel | Sequential deriving (Eq,Show)

countryJson = "../data/full/countries.json"
stateJson = "../data/full/states_provinces.json"
separatePath = "../data/separate/"

evaluate :: Execution -> Execution -> Execution -> Int -> Int -> IO ()
evaluate e1 e2 e3 numPoints additionalEntities = do
    putStrLn ("Starting Evaluation with " ++ show numPoints
                ++ " points and " ++ show additionalEntities ++
                " additional entities")
    putStrLn "Generating test points using "
    let points = generateTestPoints numPoints
    putStrLn $ "Generated " ++ (show $ length points) ++ " points"
    putStrLn ("Loading test entities using " ++ show e1 ++ " mode")
    seedEntities <- loadTestEntities e1
    putStrLn $ "Loaded " ++ (show $ length seedEntities) ++ " test entities"
    putStrLn "Generating additional entities"
    let generatedEntities = generateNewEntities e1 seedEntities additionalEntities
        entities = seedEntities ++ generatedEntities
    putStrLn $ (show $ length entities) ++ " total entities"
    putStrLn ("Constructing RTree using " ++ show e2 ++ " mode" )
    let tree = makeTree e2 entities
    putStrLn $ "Constructed RTree of depth " ++ (show $ RT.depth tree)
    putStrLn $ "Query points using " ++ show e3 ++ " mode"
    let results = case e3 of
                    Sequential -> op
                    Parallel -> op `using` parList rdeepseq
                    where op = map (\p -> filter (isContain p) $ RT.contains tree p) points
                          isContain p leaf = E.containsPoint (RT.getElem leaf) p
    putStrLn "Length of results:"
    print $ length results

evaluateList :: Execution -> [Point] -> IO ()
evaluateList e points = do
    entities <- loadTestEntities e
    let tree = makeTree e entities
        result = case e of 
                   Sequential -> op
                   Parallel -> op `using` parList rdeepseq
                   where op = map (\p -> filter (isContain p) $ RT.contains tree p) points
                         isContain p leaf = E.containsPoint (RT.getElem leaf) p
    mapM_ print $ zip points result 

loadTestEntities :: Execution -> IO [E.Entity]
loadTestEntities Sequential = do
    countries <- loadCountries countryJson
    states <- loadStates stateJson
    return (countries ++ states)
loadTestEntities Parallel = do
    filePaths <- listDirectory separatePath
    let paths = filter (\path -> path `notElem` [".DS_Store"]) filePaths
    es <- withPool 4 $ \pool -> parallel pool (map load paths)
    return $ concat es


load :: String -> IO [E.Entity]
load path@('s': _) = loadStates $ separatePath ++ path
load path@('c': _) = loadCountries $ separatePath ++ path

loadStates :: String -> IO [E.Entity]
loadStates path = do
    x <- B.readFile path
    case parseFeatureCollection x of
        Nothing -> error "error parsing feature collection"
        Just fc -> case E.parseStates fc of
                       Nothing -> error "error parsing states"
                       Just states -> return states 

loadCountries :: String -> IO [E.Entity]
loadCountries path = do
    x <- B.readFile path
    case parseFeatureCollection x of
        Nothing -> error "error parsing feature collection"
        Just fcs -> case E.parseCountries fcs of
                        Nothing -> error "error parsing states"
                        Just countries -> return countries

generateTestPoints :: Int -> [Point]
generateTestPoints n = G.genPoints world n

generateNewEntities :: Execution -> [E.Entity] -> Int -> [E.Entity]
generateNewEntities e bounds numEntities = genList ++ remList 
  where num = numEntities `quot` length bounds
        r = numEntities `mod` length bounds
        remList 
         | e == Sequential = concat (map (generateEntity 1) (take r bounds))
         | otherwise = concat (map (generateEntity 1) (take r bounds) `using` parList rdeepseq)
        genList
         | e == Sequential = concat (map (generateEntity num) bounds)
         | otherwise = concat (map (generateEntity num) bounds `using` parList rdeepseq)

generateEntity :: Int -> E.Entity -> [E.Entity]
generateEntity n entity = E.buildEntityWithGeo <$> (G.genPolygons n (getBoundingBox entity))

makeTree :: (Boundable a, NFData a) => Execution -> [a] -> RT.RTree a
makeTree Sequential xs = RT.fromList xs
makeTree Parallel xs = let chunks = split numChunks xs in makeTree_Strat chunks 
 where numChunks = 10

makeTree_Strat :: (Boundable a, NFData a) => [[a]] -> RT.RTree a
makeTree_Strat entitiess = foldr1 RT.union (map RT.fromList entitiess `using` parList rdeepseq)

split :: Int -> [a] -> [[a]]
split numChunks xs = chunksOf (length xs `quot` numChunks) xs

world :: BoundingBox
world = BoundingBox { x1 = longMin
                    , y1 = latMin
                    , x2 = longMax
                    , y2 = latMax
                    }
                    where latMin = -90
                          latMax = 90
                          longMin = -180
                          longMax = 180
