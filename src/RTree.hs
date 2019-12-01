module RTree where

import BoundingBox (BoundingBox, Boundable, getBoundingBox,Point)
import qualified BoundingBox as BB
import Data.List (sortBy)

maxChildren = 3

data RTree a =
    Node BoundingBox [RTree a]
  | Leaf BoundingBox a
  | Empty
  deriving (Show)

instance Boundable (RTree a) where
    getBoundingBox (Node bb _) = bb
    getBoundingBox (Leaf bb _) = bb
    getBoundingBox Empty = error "getBoundingBox on Empty"

newTree :: RTree a
newTree = Empty

insert :: Boundable a => RTree a -> a -> RTree a
insert Empty elem = Leaf (getBoundingBox elem) elem
insert e@(Leaf bb _) elem = Node (BB.enlarge bb $ getBoundingBox elem) [e, insert Empty elem]
insert (Node bb children) elem
    | length children == maxChildren = Node enlargedBox updatedChildren
    | otherwise = Node enlargedBox appendedChildren
    where enlargedBox = BB.enlarge bb $ getBoundingBox elem
          appendedChildren = insert Empty elem:children
          updatedChildren = let (hd:tl) = (sortBy compare' $ children) in
                            (insert hd elem):tl
          compare' x y = computeBBDiff x `compare` computeBBDiff y
          computeBBDiff x = enlargedArea x - originalArea x
          originalArea = BB.area . getBoundingBox
          enlargedArea = BB.area . (BB.enlarge $ getBoundingBox elem) . getBoundingBox

-- fromList :: [(BoundingBox, a)] -> RTree a

-- splitNode :: RTree a -> [RTree a]
-- splitNode Empty = error "split empty node"
-- splitNode Leaf _ _ = error "split leaf node"
-- splitNode Node bb children = 

contains :: RTree a -> Point -> [RTree a]
contains Empty _ = []
contains l@(Leaf bb a) p 
 | BB.containsPoint bb p = [l]
 | otherwise = []
contains (Node bb children) p
 | BB.containsPoint bb p = foldr (\x acc ->(contains x p) ++ acc) [] children
 | otherwise = []
