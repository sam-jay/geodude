{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

{-  References:

    https://artyom.me/aeson
    https://www.williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html
    https://stackoverflow.com/questions/1165647/how-to-determine-if-a-list-of-polygon-points-are-in-clockwise-order
    http://geomalgorithms.com/a03-_inclusion.html
-}

module Geometry (
    Geometry (..),
    LinearRing (..),
    GeoError,
    containsP,
    fromLineString,
    Point
) where

import Data.Aeson
import BoundingBox ( BoundingBox(..)
                   , Boundable
                   , getBoundingBox
                   , enlarge
                   )
import GHC.Generics (Generic)
import Control.DeepSeq

data GeoError =
    ClockwiseOuterRing { badRing :: LinearRing }
  | CounterClockwiseInnerRing
  | LineStringTooShort
  | LineStringNotClosed
  | UnknownGeometryType

instance Show GeoError where
    show ClockwiseOuterRing { badRing } =
        "Polygon has invalid clockwise outer ring: " ++ show badRing
    show CounterClockwiseInnerRing =
        "Polygon has invalid counterclockwise inner ring(s)."
    show LineStringTooShort = "LineString too short."
    show LineStringNotClosed = "LineString not closed."
    show UnknownGeometryType = "Unknown geometry type."

data Geometry =
    Polygon { pOuterRing :: LinearRing
            , pInnerRings :: [LinearRing] }
  | MultiPolygon { mPolygons :: [Geometry] }
  deriving (Show, Eq, Generic)

instance NFData Geometry

instance Boundable Geometry where
    getBoundingBox Polygon { pOuterRing } = getBoundingBox pOuterRing
    getBoundingBox MultiPolygon { mPolygons } = foldl1 enlarge $
        map getBoundingBox mPolygons

instance FromJSON Geometry where
    parseJSON = withObject "Geometry" $ \obj -> do
        _type <- obj .: "type"
        case _type of
            String "Polygon" ->
                do linearRings <- obj .: "coordinates"
                   return $ unwrap $ fromLinearRings linearRings
            String "MultiPolygon" ->
                do linearRingsList <- obj .: "coordinates"
                   let polygons = fromLinearRings <$> linearRingsList
                   return $ MultiPolygon { mPolygons = fmap unwrap polygons }
            _ -> error $ show UnknownGeometryType

unwrap :: Show a => Either a b -> b
unwrap (Left e) = error $ show e
unwrap (Right p) = p

{-  A linear ring MUST follow the right-hand rule with respect to the
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

isClockwise :: LinearRing -> Bool
isClockwise = (> 0) . sum . map transformEdge . makeEdges . getLineString
    where transformEdge ((x1, y1), (x2, y2)) = (x2 - x1) * (y2 + y1)
          makeEdges = zip <$> id <*> tail

-- Check whether a polygon contains a point using winding number algo
windNum :: LinearRing -> Point -> Bool
windNum rs (x, y) = (/= zero) . sum $ map checkOneEdge edges
 where zero = 0 :: Int
       edges = makeEdges $ getLineString rs
       makeEdges ls = zip ls (tail ls)
       isLeft (x1, y1) (x2, y2)
            | y1 < y2 = crossProduct > 0
            | y1 > y2 = crossProduct < 0
            | otherwise = False
            where crossProduct = ((x2 - x1) * (y - y1))
                                    - ((x - x1) * (y2 - y1))
       checkOneEdge (p1@(_, y1), p2@(_, y2))
            | y1 <= y && y2 > y && isLeft p1 p2 = 1
            | y1 > y && y2 <= y && isLeft p1 p2 = -1
            | otherwise = 0

containsP :: Point -> Geometry -> Bool
containsP p (Polygon {pOuterRing}) = windNum pOuterRing p
containsP p (MultiPolygon {mPolygons}) = any (containsP p) mPolygons

newtype LinearRing = LinearRing { getLineString :: LineString
                                } deriving (Show, Eq, Generic)

instance NFData LinearRing

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
        return $ unwrap $ fromLineString ls

-- A linear ring is a closed LineString with four or more positions.
fromLineString :: LineString -> Either GeoError LinearRing
fromLineString ls
    | length ls < 4 = Left LineStringTooShort
    | not $ isClosedLineString ls = Left LineStringNotClosed
    | otherwise = Right $ LinearRing ls

isClosedLineString :: LineString -> Bool
isClosedLineString ls
    | [] <- ls = True
    | [_] <- ls = True
    | [x, y] <- ls, x /= y = False
    | [x, y] <- ls, x == y = True
    | x:_:rest <- ls = isClosedLineString (x:rest)

type LineString = [Point]

type Point = (Double, Double)
