module Evaluate where

import Entities (Entity)
import Geometry (Geometry, Point)
import qualified RTree as RT
import Control.Parallel.Strategies (using, parList, rdeepseq)
import qualified Generator as G
import BoundingBox (BoundingBox(..), Boundable)

data Execution = Parallel | Sequential deriving (Show)

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
                    where op = map (RT.contains tree) points
    putStrLn $ show $ length results


loadTestEntities :: Execution -> IO [Entity]
loadTestEntities e = do
    return [] :: IO [Entity] -- TODO: finish this

generateTestPoints :: Int -> [Point]
generateTestPoints n = G.genPoints world n

generateNewEntities :: Execution -> [Entity] -> Int -> [Entity]
generateNewEntities e bounds numEntities = [] -- TODO: finish this

makeTree :: Boundable a => Execution -> [a] -> RT.RTree a
makeTree Sequential xs = RT.fromList xs
makeTree Parallel xs = RT.fromList xs -- TODO: finish this

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
