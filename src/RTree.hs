module RTree where

maxChildren = 3

class Bounded a where
    getBoundingBox :: a -> Rectangle

data RTree a =
    Node [RTree a]
  | Leaf a
  | Empty


newTree :: RTree
newTree = Empty

insert :: Bounded a => RTree a -> a -> RTree a
insert Empty elem = Leaf elem
insert a@(Leaf item) elem = Node [a, Leaf elem]
insert (Node children) elem = addElem bestChild elem
                              where bestChild = snd $ minimum $ zip (map (computeD elem) children) children
                                    computeD e1 e2 = a1 - area $ getBoundingBox e2
                                    a1 = area $ enlargedBox (getBoundingBox e1) (getBoundingBox e2)
                                    area (x1, y1) (x2, y2) = (x2-x1) * (y2-y1)
                                    enlargedBox ((x1,y1), (x2,y2)) ((x3,y3), (x4,y4)) = let topLeft = (min x1 x3, min y1 y3)
                                                                                            bottomRight = (max x2 x4, max y2 y4)
                                                                                        in (topLeft, bottomRight)

addElem :: Bounded a => RTree a -> a -> RTree a


get :: RTree a -> Point -> [a]


data Node =
    Node { nEntries :: [Entry] }


data Entry =
    Entry { eBoundingBox :: Rectangle
          , eChild :: Node }


type Rectangle = (Point, Point)

type Point = (Float, Float)