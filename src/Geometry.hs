{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- https://artyom.me/aeson
-- https://www.williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html

module Geometry (
    Geometry,
    GeoError,
    containsP
) where

import Data.Aeson
import Data.Monoid
import BoundingBox (BoundingBox(BoundingBox), Boundable, getBoundingBox)
import qualified BoundingBox as BB

data GeoError =
    ClockwiseOuterRing { badRing :: LinearRing }
  | CounterClockwiseInnerRing
  | LineStringTooShort
  | LineStringNotClosed
  | UnknownGeometryType

instance Show GeoError where
    show ClockwiseOuterRing { badRing } = "Polygon has invalid clockwise outer ring: " ++ show badRing
    show CounterClockwiseInnerRing = "Polygon has invalid counterclockwise inner ring(s)."
    show LineStringTooShort = "LineString too short."
    show LineStringNotClosed = "LineString not closed."
    show UnknownGeometryType = "Unknown geometry type."

data Geometry =
    Polygon { pOuterRing :: LinearRing
            , pInnerRings :: [LinearRing] }
  | MultiPolygon { mPolygons :: [Geometry] }
  deriving (Show, Eq)


instance Boundable Geometry where
    getBoundingBox Polygon { pOuterRing } = getBoundingBox pOuterRing
    getBoundingBox MultiPolygon { mPolygons } = foldl1 BB.enlarge (map getBoundingBox mPolygons)


instance FromJSON Geometry where
    parseJSON = withObject "Geometry" $ \obj -> do
        _type <- obj .: "type"
        case _type of
            String "Polygon" -> do linearRings <- obj .: "coordinates"
                                   case fromLinearRings linearRings of
                                       Left e -> error $ show e
                                       Right p -> return $ p
            String "MultiPolygon" -> do linearRingsList <- obj .: "coordinates"
                                        let polygons = fromLinearRings <$> linearRingsList :: [Either GeoError Geometry]
                                        return $ MultiPolygon { mPolygons = fmap extract polygons }
                                        where extract :: Either GeoError Geometry -> Geometry
                                              extract (Left e) = error $ show e
                                              extract (Right p) = p
            _ -> error $ show UnknownGeometryType

{-
A linear ring MUST follow the right-hand rule with respect to the
area it bounds, i.e., exterior rings are counterclockwise, and
holes are clockwise.
-}
fromLinearRings :: [LinearRing] -> Either GeoError Geometry
fromLinearRings rings
    | isClockwise outerRing = Left $ ClockwiseOuterRing { badRing = outerRing }
    | anyCounterClockwise innerRings = Left CounterClockwiseInnerRing
    | otherwise = Right $ Polygon { pOuterRing = outerRing
                                  , pInnerRings = innerRings }
    where outerRing = head rings
          innerRings = tail rings
          anyCounterClockwise = any (not . isClockwise)


-- https://stackoverflow.com/questions/1165647/how-to-determine-if-a-list-of-polygon-points-are-in-clockwise-order
isClockwise :: LinearRing -> Bool
isClockwise = (> 0) . sum . map transformEdge . makeEdges . getLineString
    where transformEdge ((x1, y1), (x2, y2)) = (x2 - x1) * (y2 + y1)
          makeEdges = zip <$> id <*> tail

-- check whether a geometry contains a point using winding number algo: 
-- http://geomalgorithms.com/a03-_inclusion.html
windNum :: LinearRing -> Point -> Bool
windNum rs (x, y) = (/= 0) . sum $ map checkOneEdge edges
 where edges = makeEdges $ getLineString rs
       makeEdges ls = zip ls (tail ls)
       isLeft (x1, y1) (x2, y2) 
        | y1 < y2 = crossProduct > 0
        | y1 > y2 = crossProduct < 0
        | otherwise = False
        where crossProduct = ((x2 - x1) * (y - y1)) - ((x - x1) * (y2 - y1))
       checkOneEdge (p1@(x1, y1), p2@(x2, y2))
        | y1 <= y && y2 > y && isLeft p1 p2 = 1
        | y1 > y && y2 <= y && isLeft p1 p2 = -1
        | otherwise = 0

containsP :: Geometry -> Point -> Bool
containsP (Polygon {pOuterRing}) p = windNum pOuterRing p
containsP (MultiPolygon {mPolygons}) p = any (\geo -> windNum (pOuterRing geo) p) mPolygons


newtype LinearRing = LinearRing { getLineString :: LineString } deriving (Show,Eq)

instance Boundable LinearRing where
    getBoundingBox LinearRing { getLineString } 
     | minX > maxX || minY > maxY = error "Invalid BoundingBox"
     | otherwise = BoundingBox minX minY maxX maxY
        where minX = minimum $ xs
              maxX = maximum $ xs
              minY = minimum $ ys
              maxY = maximum $ ys
              xs = map fst getLineString
              ys = map snd getLineString

instance FromJSON LinearRing where
    parseJSON jsn = do
        ls <- parseJSON jsn
        case fromLineString ls of
            Left e -> fail $ show e
            Right lr -> return lr

{- A linear ring is a closed LineString with four or more positions. -}
fromLineString :: LineString -> Either GeoError LinearRing
fromLineString ls
    | length ls < 4 = Left LineStringTooShort
    | not $ isClosedLineString ls = Left LineStringNotClosed
    | otherwise = Right $ LinearRing ls

isClosedLineString :: LineString -> Bool
isClosedLineString ls
    | [] <- ls = True
    | [x] <- ls = True
    | [x, y] <- ls, x /= y = False
    | [x, y] <- ls, x == y = True
    | x:y:rest <- ls = isClosedLineString (x:rest)

type LineString = [Point]

type Point = (Double, Double)

