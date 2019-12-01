module BoundingBox where

import Data.List (intersperse)

data BoundingBox = BoundingBox { x1 :: !Double
                               , y1 :: !Double
                               , x2 :: !Double
                               , y2 :: !Double } deriving (Eq)

type Point = (Double, Double)

--get the least enlargement boundingbox that contains bb1 and bb2
enlarge :: BoundingBox -> BoundingBox -> BoundingBox
enlarge b1 b2 = BoundingBox (min x1 x1') (min y1 y1') (max x2 x2') (max y2 y2')
    where BoundingBox x1 y1 x2 y2 = b1
          BoundingBox x1' y1' x2' y2' = b2

-- compute the area of a boundingbox
area :: BoundingBox -> Double
area (BoundingBox x1 y1 x2 y2) = (x2 - x1) * (y2 - y1)

--check whether the first boundingbox contains the second boundingbox
containsRect :: BoundingBox -> BoundingBox -> Bool
containsRect b1 b2 = x1 <= x1' && y1 <= y1' && x2 >= x2' && y2 >= y2'
    where BoundingBox x1 y1 x2 y2 = b1
          BoundingBox x1' y1' x2' y2' = b2

--check whether a boundingbox contains a point
containsPoint :: BoundingBox -> Point -> Bool
containsPoint bb (px, py) = px >= x1 && px <= x2 && py >= y1 && py <= y2
    where BoundingBox x1 y1 x2 y2 = bb

--check whether two boundingbox intersect
intersect :: BoundingBox -> BoundingBox -> Bool
intersect b1 b2 = not (x1 >= x2' || x2 <= x1' || y2 <= y1' || y2' <= y1)
    where BoundingBox x1 y1 x2 y2 = b1
          BoundingBox x1' y1' x2' y2' = b2

instance Show BoundingBox where
    show (BoundingBox x1 y1 x2 y2) = "BoundingBox [" ++ points ++ "]"
        where points = concat $ intersperse "," $ map show [x1, y1, x2, y2]

