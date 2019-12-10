{-# LANGUAGE NamedFieldPuns #-}

module Generator where

import Geometry
import qualified ConvexHull as CH
import BoundingBox (BoundingBox(..))
import System.Random
import Data.Either (fromRight)
import Data.List.Split (chunksOf)

--  http://david-kremer.fr/haskell/blogging/2018/10/28/haskell-random-number-generation.html
genRandomNumbersBetween :: Int -> Int -> (Double, Double) -> [Double]
genRandomNumbersBetween n seed (a, b) = take n $ (randomRs (a, b) myGenerator) where
    myGenerator = mkStdGen seed

getPair :: [a] -> (a, a)
getPair [x, y] = (x, y)
getPair _ = error "shouldn't happen"

genPolygons :: Int -> BoundingBox -> [Geometry]
genPolygons n (BoundingBox {x1,y1,x2,y2}) = map makePoly chunks
    where chunks = chunksOf n $ zip xs ys
          xs = genRandomNumbersBetween (numPts * n) seedX (x1, x2)
          ys = genRandomNumbersBetween (numPts * n) seedY (y1, y2)
          numPts = 20
          seedX = 100
          seedY = 120

makePoly :: [(Double, Double)] -> Geometry
makePoly pts = case lr of
                    Right x -> Polygon { pOuterRing = x, pInnerRings = [] }
                    Left m -> error $ show m
    where lr = fromLineString $ ch ++ [head ch]
          ch = map getPair $ CH.convexHull . map (\p -> [fst p, snd p]) $ pts
