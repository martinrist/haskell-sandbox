module RealWorld.Chapter03.GrahamScan where

import Data.List (sortBy, sortOn)

data Direction =
    CounterClockwise
    | Clockwise
    | Straight
    deriving (Eq, Show)

data Point = Point Double Double
            deriving (Eq, Show)

instance Ord Point where
    compare (Point x1 y1) (Point x2 y2)
        | y1 < y2 = LT
        | y1 > y2 = GT
        | x1 < x2 = LT
        | x1 > x2 = GT
        | otherwise = EQ

-- 10. Turn direction between three points

turnDirection :: (Point, Point, Point) -> Direction
turnDirection (Point x1 y1, Point x2 y2, Point x3 y3)
    | crossZ == 0 = Straight
    | crossZ > 0  = CounterClockwise
    | crossZ < 0  = Clockwise
    where crossZ = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)


-- 11. List of points -> directions between each successive triple
-- Not needed for subsqeuent implementation

pointsToDirections :: [Point] -> [Direction]
pointsToDirections = map turnDirection . inThrees
        where inThrees xs@(x1:x2:x3:_) = (x1, x2, x3) : inThrees (tail xs)
              inThrees _               = []



-- 12. Implement Graham Scan algorithm

-- Convert a list to a list of triples, where the midpoint of
-- each triple is each member of the original list in turn
-- Wraps around so that the first triple consists of (last xs, head xs, xs !! 1

-- TODO: This needs to handle the case where `length xs` < 3
toTriples :: [a] -> [(a, a, a)]
toTriples [] = []
toTriples xs = let inThrees ys@(y1:y2:y3:_) = (y1, y2, y3) : inThrees (tail ys)
                   inThrees _ = []
               in
                   inThrees ([last xs] ++ xs ++ [head xs])


midPoint :: (Point, Point, Point) -> Point
midPoint (_, y, _) = y



data Vector = Vector Double Double
    deriving (Eq, Show)

toVector :: Point -> Point -> Vector
toVector (Point x1 y1) (Point x2 y2) = Vector (x2 - x1) (y2 - y1)

angle :: Vector -> Double
angle (Vector dx dy) = atan2 dy dx

instance Ord Vector where
    compare v1 v2 = compare (angle v1) (angle v2)




-- scan a presorted set of points and remove any that are 'inside'
scanSorted :: [Point] -> [Point]
scanSorted ps = let triples = toTriples ps in
                    if any isClockwise triples
                       then let insidePoints = map midPoint $ filter isClockwise triples
                                remainingPoints = filter (`notElem` insidePoints) ps
                                in scanSorted remainingPoints
                       else ps
                where isClockwise = (== Clockwise) . turnDirection

convexHull :: [Point] -> [Point]
convexHull ps = let point1 = minimum ps
                    sortedPoints = sortOn (toVector point1) ps
                in scanSorted sortedPoints

-- Some test data

poly1 :: [Point]
poly1 = [Point 0 0, Point 3 1, Point 1 1, Point 3 3,
              Point 1 4, Point (-1) 2, Point (-2) 1]

poly2 :: [Point]
poly2 = [Point 0 0, Point 3 1, Point 2 2, Point 1.75 2, Point 2 4, Point 1 4]

poly3 :: [Point]
poly3 = [Point 1 1, Point 4 1, Point 5 3, Point 3 2, Point 2 3, Point 1 4]