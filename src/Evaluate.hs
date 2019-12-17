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
import BoundingBox (BoundingBox(..), Boundable)
import System.Directory (getDirectoryContents)
import Control.Concurrent.ParallelIO.Global

data Execution = Parallel | Sequential deriving (Show)

countryJson = "../data/full/countries.json"
stateJson = "../data/full/states_provinces.json"
chunkedJsonPath = "../data/separate"

-- Evaluate containment on a large number of points. (Sequential)
evalManyPointsSequential :: IO ()
evalManyPointsSequential = evaluate Sequential numPoints numEntities
     where numPoints = 100000
           numEntities = 0

-- Evaluate containment on a large number of points. (Parallel)
evalManyPointsParallel :: IO ()
evalManyPointsParallel = evaluate Parallel numPoints numEntities
     where numPoints = 100000
           numEntities = 0

-- Evaluate containment for a small number of points on a large tree. (Sequential)
evalLargeTreeSequential :: IO ()
evalLargeTreeSequential = evaluate Sequential numPoints numEntities
     where numPoints = 100
           numEntities = 100000

-- Evaluate containment for a small number of points on a large tree. (Parallel)
evalLargeTreeParallel :: IO ()
evalLargeTreeParallel = evaluate Parallel numPoints numEntities
     where numPoints = 100
           numEntities = 100000


evaluate :: Execution -> Int -> Int -> IO ()
evaluate e numPoints additionalEntities = do
    putStrLn ("Starting " ++ show e ++ " Evaluation with " ++ show numPoints
                ++ " points and " ++ show additionalEntities ++
                " additional entities")
    putStrLn "Generating test points"
    let points = generateTestPoints numPoints
    putStrLn $ "Generated " ++ (show $ length points) ++ " points"
    putStrLn "Loading test entities"
    seedEntities <- loadTestEntities e
    putStrLn $ "Loaded " ++ (show $ length seedEntities) ++ " test entities"
    putStrLn "Generating additional entities"
    let generatedEntities = generateNewEntities e seedEntities additionalEntities
        entities = seedEntities ++ generatedEntities
    putStrLn $ (show $ length entities) ++ " total entities"
    putStrLn "Constructing RTree"
    let tree = makeTree e entities
    putStrLn $ "Constructed RTree of depth " ++ (show $ RT.depth tree)
    let results = case e of
                    Sequential -> op
                    Parallel -> op `using` parList rdeepseq
                    where op = map (\p -> filter (isContain p) $ RT.contains tree p) points
                          isContain p leaf = E.containsPoint (RT.getElem leaf) p
    print $ length results


loadTestEntities :: Execution -> IO [E.Entity]
loadTestEntities Sequential = do
    countries <- loadCountries countryJson
    states <- loadStates stateJson
    return (countries ++ states)
loadTestEntities Parallel = do
    filePaths <- getDirectoryContents chunkedJsonPath
    let paths = filter (\path -> path `notElem` [".","..",".DS_Store"]) filePaths -- better to just check if path ends with .json?
    es <- parallel (map load paths)
    -- can't call stopGlobalPool here because it makes the entire expression of type IO ()
    -- TODO: call stopGlobalPool at the end of main or some other function that returns IO ()
    return $ concat es


load :: String -> IO [E.Entity]
load path@('s': _) = loadStates path
load path@('c': _) = loadCountries path

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
generateNewEntities e bounds numEntities = [] -- TODO: finish this

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
