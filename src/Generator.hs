{-# LANGUAGE NamedFieldPuns #-}

module Generator where

import qualified Geometry as GM
import qualified ConvexHull as CH
import BoundingBox (BoundingBox(..))
import System.Random
import Data.List.Split (chunksOf)
import qualified RTree as RT

--  http://david-kremer.fr/haskell/blogging/2018/10/28/haskell-random-number-generation.html
genRandomNumbersBetween :: Int -> Int -> (Double, Double) -> [Double]
genRandomNumbersBetween n seed (a, b) = take n $ (randomRs (a, b) myGenerator) where
    myGenerator = mkStdGen seed

getPair :: [a] -> (a, a)
getPair [x, y] = (x, y)
getPair _ = error "shouldn't happen"


genPoints :: BoundingBox -> Int -> [GM.Point]
genPoints bb n = zip xs ys
    where xs = genRandomNumbersBetween n seedX (xMin, xMax)
          ys = genRandomNumbersBetween n seedY (yMin, yMax)
          BoundingBox {x1, y1, x2, y2} = bb
          [xMin, yMin, xMax, yMax] = [x1, y1, x2, y2]
          seedX = 100
          seedY = 120


genPolygons :: Int -> BoundingBox -> [GM.Geometry]
genPolygons n (BoundingBox {x1,y1,x2,y2}) = map makePoly chunks
    where chunks = chunksOf numPts $ zip xs ys
          xs = genRandomNumbersBetween (numPts * n) seedX (x1, x2)
          ys = genRandomNumbersBetween (numPts * n) seedY (y1, y2)
          numPts = 20
          seedX = 100
          seedY = 120

genSampleTree :: RT.RTree GM.Geometry
genSampleTree = RT.fromList polygons
    where polygons = concatMap (genPolygons 10) quadrants
          quadrants = [ BoundingBox { x1 = 0, x2 = 0.49, y1 = 0, y2 = 0.49 }
                      , BoundingBox { x1 = 0.5, x2 = 1, y1 = 0, y2 = 0.49 }
                      , BoundingBox { x1 = 0, x2 = 0.49, y1 = 0.5, y2 = 1 }
                      , BoundingBox { x1 = 0.5, x2 = 1, y1 = 0.5, y2 = 1 }
                      ]

makePoly :: [(Double, Double)] -> GM.Geometry
makePoly pts = case lr of
                    Right x -> GM.Polygon { GM.pOuterRing = x, GM.pInnerRings = [] }
                    Left m -> error $ show m
    where lr = GM.fromLineString $ ch ++ [head ch]
          ch = map getPair $ CH.convexHull . map (\p -> [fst p, snd p]) $ pts
