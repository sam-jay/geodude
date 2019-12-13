module RTree where

import BoundingBox (BoundingBox, Boundable, getBoundingBox,Point)
import qualified BoundingBox as BB
import Control.Applicative ((<$>))
import Data.List (sortBy, maximumBy)
-- import Control.DeepSeq

minChildren = 2
maxChildren = 4

data RTree a =
    Node BoundingBox [RTree a]
  | Leaf BoundingBox a
  | Empty
  deriving (Show, Eq)

instance Boundable (RTree a) where
    getBoundingBox (Node bb _) = bb
    getBoundingBox (Leaf bb _) = bb
    getBoundingBox Empty = error "getBoundingBox on Empty"

-- instance NFData a => NFData (RTree a) where
--   rnf (Empty) = ()
--   rnf (Leaf _ a) = rnf a
--   rnf (Node _ children) = rnf children

newTree :: RTree a
newTree = Empty

getChildren :: RTree a -> [RTree a]
getChildren Empty = error "Empty no child"
getChildren (Leaf _ _) = error "Leaf no child"
getChildren (Node _ children) = children

singleton :: Boundable a => a -> RTree a
singleton a = Leaf (getBoundingBox a) a

-- generate a tree node which has this list of nodes as its children
generateNode :: Boundable a => [RTree a] -> RTree a
generateNode [] = Empty
generateNode children = Node newBB children
  where newBB = mergeBB' $ getBoundingBox <$> children
        mergeBB' bbs = foldr1 BB.enlarge bbs

insert :: Boundable a => RTree a -> a -> RTree a
insert Empty e = singleton e
insert n@(Leaf bb _) e = Node (mergeBB n e) [singleton e, n]
insert n@(Node bb children) e
  | length (getChildren newNode) > maxChildren = generateNode $ splitNode newNode
  | otherwise = newNode
  where newNode= addToNode n e

addToNode :: Boundable a => RTree a -> a -> RTree a
addToNode old elem = Node newBB newChildren
 where newBB = mergeBB old elem
       oldChildren = getChildren old
       directAdd = (singleton elem) : filter (\c -> (getBoundingBox c) /= (getBoundingBox elem)) oldChildren 
       newChildren 
        | depth old == 2 = directAdd
        | otherwise = insertIntoBestChild oldChildren elem

fromList :: Boundable a => [a] -> RTree a
fromList xs = foldl insert newTree xs

toList :: RTree a -> [a]
toList Empty = []
toList (Leaf _ a) = [a]
toList (Node _ ts) = concatMap toList ts

-- merge boundingbox of given node and element
mergeBB :: Boundable a => RTree a -> a -> BoundingBox
mergeBB Empty e = getBoundingBox e
mergeBB t e = BB.enlarge (getBoundingBox t) (getBoundingBox e)

-- insert element to the best child of a list of tree nodes by finding the least enlargement
-- return the list of nodes after insertion
insertIntoBestChild :: Boundable a => [RTree a] -> a -> [RTree a]
insertIntoBestChild children@(x:xs) elem
 | getBoundingBox x == getBoundingBox best = (inserted best elem) ++ xs
 | otherwise = x : insertIntoBestChild xs elem
 where (best:tl) = sortBy compare' children
       compare' x y = diffBB x `compare` diffBB y
       diffBB x = BB.area (mergeBB x elem) - originalArea x
       originalArea = BB.area . getBoundingBox
       inserted node e
        | length (getChildren newNode) > maxChildren = splitNode newNode
        | otherwise = [newNode]
        where newNode = addToNode node e

-- split a tree node into 2 nodes by regrouping its children into 2 groups
splitNode :: Boundable a => RTree a -> [RTree a]
splitNode Empty = error "split empty node"
splitNode (Leaf _ _) = error "split leaf node"
splitNode (Node bb children) = [generateNode group1, generateNode group2]
 where (l,r) = worstPair children
       toAdd = filter (\e -> (getBoundingBox e) /= (getBoundingBox l) && (getBoundingBox e) /= (getBoundingBox r)) children
       (group1, group2) = partition [l] [r] toAdd

-- find the pair of child nodes which has the biggest enlarged boundingbox
worstPair :: Boundable a => [RTree a] -> (RTree a, RTree a)
worstPair children = result
 where result = snd $ maximumBy (\m n -> compare (fst m) (fst n)) [(BB.area $ BB.enlarge (getBoundingBox c1) (getBoundingBox c2), (c1, c2)) 
                     | x <- indexedC, y <- indexedC, let (c1,idx1) = x, let (c2, idx2) = y, idx1 /= idx2]
       indexedC = zip children [1..]

-- get the enlarged boundingbox from two tree nodes
unionBB :: Boundable a => RTree a -> RTree a -> BoundingBox
unionBB n1 n2 = BB.enlarge (getBoundingBox n1) (getBoundingBox n2)

-- compute the area diff when enlarge a node to another
areaDiffWithNode :: Boundable a =>  RTree a -> RTree a -> Double
areaDiffWithNode newNode old = newArea - oldArea
 where newArea = BB.area $ unionBB newNode old
       oldArea = BB.area $ getBoundingBox old

-- partition the third list of nodes into either the first or the second group of nodes
-- return (group1, group2)
-- detailed algorithm: http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdfss
partition :: Boundable a => [RTree a] -> [RTree a] -> [RTree a] -> ([RTree a], [RTree a])
partition l r [] = (l,r)
partition l r toAdd 
 | length toAdd + length l <= minChildren = (l ++ toAdd, r)
 | length toAdd + length r <= minChildren = (l, r ++ toAdd)
 | otherwise = assign nextNode l r
 where nextNode = snd $ maximumBy (\m n -> compare (fst m) (fst n)) [(diff e, e) | e <- toAdd]
       lNode = generateNode l
       rNode = generateNode r
       leftDiff e = areaDiffWithNode e lNode
       rightDiff e = areaDiffWithNode e rNode
       diff e = abs (leftDiff e - rightDiff e)
       assignToLeft = partition (nextNode : l) r remain
       assignToRight = partition l (nextNode : r) remain
       remain = filter (\n -> (getBoundingBox n) /= (getBoundingBox nextNode)) toAdd
       assign nextNode l r
        | leftDiff nextNode < rightDiff nextNode = assignToLeft
        | leftDiff nextNode > rightDiff nextNode = assignToRight
        | BB.area (getBoundingBox lNode) < (BB.area $ getBoundingBox rNode) = assignToLeft
        | BB.area (getBoundingBox lNode) > (BB.area $ getBoundingBox rNode)= assignToRight
        | length l < length r = assignToLeft
        | otherwise = assignToRight

depth :: Boundable a => RTree a -> Int
depth Empty = 0
depth (Leaf _ _) = 1
depth (Node _ children) = 1 + (depth $ head children)

-- given a tree and a point, return all leaf nodes as a list that contain the point
contains :: Boundable a => RTree a -> Point -> [RTree a]
contains Empty _ = []
contains l@(Leaf bb a) p 
 | BB.containsPoint bb p = [l]
 | otherwise = []
contains (Node bb children) p
 | BB.containsPoint bb p = foldr (\x acc ->(contains x p) ++ acc) [] children
 | otherwise = []

--customized print function for RTree
space ::String
space = "         "

printTree :: (Boundable a, Show a) => String -> RTree a -> IO ()
printTree header Empty = putStrLn $ header ++ "Empty"
printTree header (Leaf bb x) = putStrLn $ header ++ "Leaf " ++ (show bb) ++ " " ++ (show x)
printTree header (Node bb children) = do putStrLn $ header ++ "Node " ++ (show bb) ++ "{"
                                         mapM_ (printTree (header ++ space)) children
                                         putStr "}"

