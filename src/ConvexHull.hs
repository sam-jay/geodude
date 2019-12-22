module ConvexHull where

-- http://rosettacode.org/wiki/Convex_hull#Haskell
import Data.List (sortBy, groupBy, maximumBy)
import Data.Ord (comparing)

x :: [a] -> a
y :: [b] -> b
(x, y) = ((!! 0), (!! 1))

compareFrom :: (Num a, Ord a) => [a] -> [a] -> [a] -> Ordering
compareFrom o l r =
  compare ((x l - x o) * (y r - y o)) ((y l - y o) * (x r - x o))

distanceFrom :: Floating a => [a] -> [a] -> a
distanceFrom from to = ((x to - x from) ** 2 + (y to - y from) ** 2) ** (1 / 2)

convexHull :: (Floating a, Ord a) => [[a]] -> [[a]]
convexHull points =
  let o = minimum points
      presorted = sortBy (compareFrom o) (filter (/= o) points)
      collinears = groupBy (((EQ ==) .) . compareFrom o) presorted
      outmost = maximumBy (comparing (distanceFrom o)) <$> collinears
  in dropConcavities [o] outmost

dropConcavities :: (Num a, Ord a) => [[a]] -> [[a]] -> [[a]]
dropConcavities (left:lefter) (right:righter:rightest) =
  case compareFrom left right righter of
    LT -> dropConcavities (right : left : lefter) (righter : rightest)
    EQ -> dropConcavities (left : lefter) (righter : rightest)
    GT -> dropConcavities lefter (left : righter : rightest)
dropConcavities output lastInput = lastInput ++ output
