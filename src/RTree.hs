{-# LANGUAGE DeriveGeneric #-}

{-  References:

    http://hackage.haskell.org/package/data-r-tree-0.0.5.0/docs/Data-RTree.html
    http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdfss
-}

module RTree where

import BoundingBox
import Data.List (sortBy, maximumBy)
import GHC.Generics (Generic)
import Control.DeepSeq

minChildren :: Int
minChildren = 2

maxChildren :: Int
maxChildren = 4

data RTree a =
    Node BoundingBox [RTree a]
  | Leaf BoundingBox a
  | Empty
  deriving (Eq, Generic)

instance NFData a => NFData (RTree a)

instance Boundable (RTree a) where
    getBoundingBox (Node bb _) = bb
    getBoundingBox (Leaf bb _) = bb
    getBoundingBox Empty = error "getBoundingBox on Empty"

instance Show a => Show (RTree a) where
    show Empty = "Empty"
    show (Leaf _ e) = show e
    show (Node _ children) = show children

newTree :: RTree a
newTree = Empty

getChildren :: RTree a -> [RTree a]
getChildren (Node _ children) = children
getChildren _ = []

getElem :: Boundable a => RTree a -> a
getElem Empty = error "getElem on Empty"
getElem (Leaf _ e) = e
getElem (Node _ _) = error "Node does not have elem"

singleton :: Boundable a => a -> RTree a
singleton a = Leaf (getBoundingBox a) a

-- Generate a node which has this list of nodes as its children
generateNode :: Boundable a => [RTree a] -> RTree a
generateNode [] = Empty
generateNode children = Node newBB children
  where newBB = mergeBB' $ getBoundingBox <$> children
        mergeBB' bbs = foldr1 enlarge bbs

insert :: Boundable a => RTree a -> a -> RTree a
insert Empty e = singleton e
insert n@(Leaf _ _) e = Node (mergeBB n e) [singleton e, n]
insert n@(Node _ _) e
    | length (getChildren newN) > maxChildren = generateNode $ splitNode newN
    | otherwise = newN
    where newN = addToNode n $ singleton e

-- Merge two subtrees into one
union :: Boundable a => RTree a -> RTree a -> RTree a
union Empty right = right
union left Empty = left
union l@(Leaf bb1 _) r@(Leaf bb2 _)
    | bb1 == bb2 = l -- if two leaves have the same bounding box, return left
    | otherwise = generateNode [l,r]
union left right
    | depth left > depth right = union right left
    | depth left == depth right = foldr1 union $ (getChildren left) ++ [right]
    | length (getChildren newN) > maxChildren = generateNode $ splitNode newN
    | otherwise = newN
    where newN = addToNode right left

-- Add new node to a tree
addToNode :: Boundable a => RTree a -> RTree a -> RTree a
addToNode old new = Node newBB newChildren
 where newBB = unionBB old new
       oldChildren = getChildren old
       directAdd = new : filter (bbNotSame new) oldChildren
       bbNotSame n c = getBoundingBox c /= getBoundingBox n
       newChildren 
        | depth old == depth new + 1 = directAdd
        | otherwise = insertIntoBestChild oldChildren new

fromList :: Boundable a => [a] -> RTree a
fromList xs = foldl insert newTree xs

toList :: RTree a -> [a]
toList Empty = []
toList (Leaf _ a) = [a]
toList (Node _ ts) = concatMap toList ts

-- Merge boundingbox of given node with element
mergeBB :: Boundable a => RTree a -> a -> BoundingBox
mergeBB Empty e = getBoundingBox e
mergeBB t e = enlarge (getBoundingBox t) (getBoundingBox e)

{- Insert a new node into the best child of a list of tree nodes by finding
    the child that needs to expand its bounding box the least to accommodate
    the new node.
-}
insertIntoBestChild :: Boundable a => [RTree a] -> RTree a -> [RTree a]
insertIntoBestChild [] _ = []
insertIntoBestChild children@(x:xs) new
    | getBoundingBox x == getBoundingBox best = (inserted best) ++ xs
    | otherwise = x : insertIntoBestChild xs new
    where (best:_) = sortBy compare' children
          compare' x' y = diffBB x' `compare` diffBB y
          diffBB x' = area (unionBB x' new) - originalArea x'
          originalArea = area . getBoundingBox
          inserted node
            | length (getChildren newNode) > maxChildren = splitNode newNode
            | otherwise = [newNode]
            where newNode = addToNode node new

-- Split a tree node into 2 nodes by regrouping its children into 2 groups
splitNode :: Boundable a => RTree a -> [RTree a]
splitNode Empty = error "cannot split empty node"
splitNode (Leaf _ _) = error "cannot split leaf node"
splitNode (Node _ children) = [generateNode group1, generateNode group2]
 where (l,r) = worstPair children
       toAdd = filter notLOrR children
       notLOrR e = getBoundingBox e /= getBoundingBox l &&
                        getBoundingBox e /= getBoundingBox r
       (group1, group2) = partition [l] [r] toAdd

-- Find the pair of child nodes which form the biggest enlarged boundingbox
worstPair :: Boundable a => [RTree a] -> (RTree a, RTree a)
worstPair children = result
 where result = snd $ maximumBy (\m n -> compare (fst m) (fst n)) $
            [ (combinedArea, pair)
            | x <- indexedC
            , y <- indexedC
            , let (c1, idx1) = x
                  (c2, idx2) = y
            , idx1 /= idx2
            , let bb1 = getBoundingBox c1
                  bb2 = getBoundingBox c2
                  combinedArea = area $ enlarge bb1 bb2
                  pair = (c1, c2)
            ]
       indexedC = zip children ([1..] :: [Int])

-- Get the enlarged boundingbox containing two nodes
unionBB :: Boundable a => RTree a -> RTree a -> BoundingBox
unionBB n1 n2 = enlarge (getBoundingBox n1) (getBoundingBox n2)

-- Compute the area diff when merging a node with another
areaDiffWithNode :: Boundable a =>  RTree a -> RTree a -> Double
areaDiffWithNode newNode old = newArea - oldArea
 where newArea = area $ unionBB newNode old
       oldArea = area $ getBoundingBox old

-- Partition the third list of nodes into either the first
-- or the second group of nodes returning (group1, group2)
partition
    :: Boundable a
    => [RTree a] -> [RTree a] -> [RTree a] -> ([RTree a], [RTree a])
partition l r [] = (l,r)
partition l r toAdd 
 | length toAdd + length l <= minChildren = (l ++ toAdd, r)
 | length toAdd + length r <= minChildren = (l, r ++ toAdd)
 | otherwise = assign nextNode l r
 where nextNode = snd $ maximumBy (\m n -> compare (fst m) (fst n)) $
            [(diff e, e) | e <- toAdd]
       lNode = generateNode l
       rNode = generateNode r
       leftDiff e = areaDiffWithNode e lNode
       rightDiff e = areaDiffWithNode e rNode
       diff e = abs (leftDiff e - rightDiff e)
       assignToLeft = partition (nextNode : l) r remain
       assignToRight = partition l (nextNode : r) remain
       remain = filter notNextNode toAdd
       notNextNode n = getBoundingBox n /= getBoundingBox nextNode
       assign nextN l' r'
            | leftDiff nextN < rightDiff nextN = assignToLeft
            | leftDiff nextN > rightDiff nextN = assignToRight
            | areaL < areaR = assignToLeft
            | areaL > areaR = assignToRight
            | length l' < length r' = assignToLeft
            | otherwise = assignToRight
            where areaL = area $ getBoundingBox lNode
                  areaR = area $ getBoundingBox rNode

depth :: Boundable a => RTree a -> Int
depth Empty = 0
depth (Leaf _ _) = 1
depth (Node _ children) = 1 + (maximum $ map depth children)

-- Get all leaf nodes as a list that contain the point
contains :: Boundable a => RTree a -> Point -> [RTree a]
contains Empty _ = []
contains l@(Leaf bb _) p
    | containsPoint bb p = [l]
    | otherwise = []
contains (Node bb children) p
    | containsPoint bb p = foldr (\x acc -> contains x p ++ acc) [] children
    | otherwise = []

printTree :: (Boundable a, Show a) => String -> RTree a -> IO ()
printTree header Empty = putStrLn $ header ++ "Empty"
printTree header (Leaf bb x) = putStrLn $
    header ++ "Leaf " ++ show bb ++ " " ++ show x
printTree header (Node bb children) =
    do putStrLn $ header ++ "Node " ++ (show bb) ++ "{"
       mapM_ (printTree $ header ++ space) children
       putStr "}"
    where space = replicate 9 ' '
