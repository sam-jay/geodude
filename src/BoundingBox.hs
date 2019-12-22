{-# LANGUAGE DeriveGeneric #-}

module BoundingBox where

import Data.List (intersperse)
import GHC.Generics (Generic)
import Control.DeepSeq

data BoundingBox = BoundingBox { x1 :: !Double
                               , y1 :: !Double
                               , x2 :: !Double
                               , y2 :: !Double } deriving (Eq, Generic)

instance NFData BoundingBox

instance Ord BoundingBox where
    bb1 `compare` bb2 = area bb1 `compare` area bb2

class Boundable a where
    getBoundingBox :: a -> BoundingBox

type Point = (Double, Double)

-- Get the smallest bounding box that contains the two input bounding boxes
enlarge :: BoundingBox -> BoundingBox -> BoundingBox
enlarge b1 b2 = BoundingBox (min x1' x1'') (min y1' y1'')
                            (max x2' x2'') (max y2' y2'')
    where BoundingBox x1' y1' x2' y2' = b1
          BoundingBox x1'' y1'' x2'' y2'' = b2

-- Compute the area of a bounding box
area :: BoundingBox -> Double
area (BoundingBox x1' y1' x2' y2') = (x2' - x1') * (y2' - y1')

-- Check whether a bounding box contains a point
containsPoint :: BoundingBox -> Point -> Bool
containsPoint bb (px, py) = px > x1' && px < x2' && py > y1' && py < y2'
    where BoundingBox x1' y1' x2' y2' = bb

instance Show BoundingBox where
    show (BoundingBox x1' y1' x2' y2') = "BB [" ++ points ++ "]"
        where points = concat $ intersperse "," $ map show [x1', y1', x2', y2']
