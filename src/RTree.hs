module RTree where

import BoundingBox (BoundingBox, Boundable, getBoundingBox,Point)
import qualified BoundingBox as BB
import Control.Applicative ((<$>))
import Data.List (sortBy, maximumBy)

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

newTree :: RTree a
newTree = Empty

singleton :: Boundable a => a -> RTree a
singleton a = Leaf (getBoundingBox a) a

generateNode :: Boundable a => [RTree a] -> RTree a
generateNode children = Node (mergeBB' $ getBoundingBox <$> children) children
 where mergeBB' [] = error "empty bounding box list"
       mergeBB' bbs = foldr1 BB.enlarge bbs

insert :: Boundable a => RTree a -> a -> RTree a
insert Empty e = singleton e
insert n@(Leaf bb _) e = Node (mergeBB n e) [singleton e, n]
insert n@(Node bb children) e
 | length newChildren > maxChildren = generateNode $ splitNode newNode
 | otherwise = newNode
 where newNode@(Node newBB newChildren) = Node (mergeBB n e) $ insertIntoBestChild children e


fromList :: Boundable a => [a] -> RTree a
fromList xs = foldl insert newTree xs

toList :: RTree a -> [a]
toList Empty = []
toList (Leaf _ a) = [a]
toList (Node _ ts) = concat $ map toList ts

mergeBB :: Boundable a => RTree a -> a -> BoundingBox
mergeBB Empty e = getBoundingBox e
mergeBB t e = BB.enlarge (getBoundingBox t) (getBoundingBox e)


insertIntoBestChild :: Boundable a => [RTree a] -> a -> [RTree a]
insertIntoBestChild children elem = (insert hd elem) : tl
 where (hd:tl) = sortBy compare' children
       compare' x y = diffBB x `compare` diffBB y
       diffBB x = enlargedArea x - originalArea x
       originalArea = BB.area . getBoundingBox
       enlargedArea = BB.area . (BB.enlarge $ getBoundingBox elem) . getBoundingBox

splitNode :: Boundable a => RTree a -> [RTree a]
splitNode Empty = error "split empty node"
splitNode (Leaf _ _) = error "split leaf node"
splitNode (Node bb children) = [generateNode group1, generateNode group2]
 where (l,r) = worstPair children
       toAdd = filter (\e -> (getBoundingBox e) /= (getBoundingBox l) && (getBoundingBox e) /= (getBoundingBox r)) children
       (group1, group2) = partition [l] [r] toAdd

worstPair :: Boundable a => [RTree a] -> (RTree a, RTree a)
worstPair children = result
 where result = snd $ maximumBy (\m n -> compare (fst m) (fst n)) [(BB.area $ BB.enlarge (getBoundingBox c1) (getBoundingBox c2), (c1, c2)) 
                     | x <- indexedC, y <- indexedC, let (c1,idx1) = x, let (c2, idx2) = y, idx1 /= idx2]
       indexedC = zip children [1..]

unionBB :: Boundable a => RTree a -> RTree a -> BoundingBox
unionBB n1 n2 = BB.enlarge (getBoundingBox n1) (getBoundingBox n2)

areaDiffWithNode :: Boundable a =>  RTree a -> RTree a -> Double
areaDiffWithNode newNode old = newArea - oldArea
 where newArea = BB.area $ unionBB newNode old
       oldArea = BB.area $ getBoundingBox old

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

fromList :: Boundable a => [a] -> RTree a
fromList [] = Empty
fromList [x] = singleton x
fromList xs = foldr (\x acc -> insert acc x) newTree xs

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

