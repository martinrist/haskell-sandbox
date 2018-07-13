module Exercises where

import Data.List (sortBy, minimumBy, sortOn)

-- 1. Function that computes the number of elements in a list
-- 2. Type signature for exercise 1

elements :: [a] -> Int
elements [] = 0
elements (_:xs) = 1 + elements xs


-- 3. Function that computes the mean of a list

mean :: [Int] -> Double
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)


-- 4. Turns a list into a palindrome

toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ reverse xs


-- 5. Test whether a list is a palindrome

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs


-- 6. Sorts a list of lists based on the length of each sublist

sortBySublistLength :: [[a]] -> [[a]]
sortBySublistLength = sortBy lengthComparator
    where lengthComparator xs ys = compare (length xs) (length ys)


-- 7. Joins a list of lists togetehr using a seprator value

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [l] = l
intersperse s (l:ls) = l ++ [s] ++ intersperse s ls


-- 8. Write function to determine height of a binary tree

data Tree a
    = Node a (Tree a) (Tree a)
    | Empty
    deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ t1 t2) = 1 + max (height t1) (height t2)


-- 9. Direction datatype 

data Direction =
    CounterClockwise
    | Clockwise
    | Straight
    deriving (Eq, Show)

data Point = Point Double Double
            deriving (Eq, Show)


-- 10. Turn direction between three points

turnDirection :: (Point, Point, Point) -> Direction
turnDirection (Point x1 y1, Point x2 y2, Point x3 y3)
    | crossZ == 0 = Straight
    | crossZ > 0  = CounterClockwise
    | crossZ < 0  = Clockwise
    where crossZ = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)


-- 11. List of points -> directions between each successive triple

pointsToDirections :: [Point] -> [Direction]
pointsToDirections = map turnDirection . inThrees


inThrees :: [a] -> [(a, a, a)]
inThrees xs@(x1:x2:x3:_) = (x1, x2, x3) : inThrees (tail xs)
inThrees _               = []


-- 12. Implement Graham Scan algorithm

midPoint :: (Point, Point, Point) -> Point
midPoint (_, y, _) = y



-- Find the starting point for the algorithm
-- This is the point with lowest y-coordinate, with ties decided
-- by lowest x-coordinate
initialPoint :: [Point] -> Point
initialPoint = minimumBy pointsComparator
    where pointsComparator (Point x1 y1) (Point x2 y2)
            | y1 < y2 = LT
            | y2 > y1 = GT
            | x1 < x2 = LT
            | x1 > x2 = GT
            | otherwise = EQ


-- Sort the points by the angle that they and the first point make with the x-axis
sortedByAngleWithXAxis :: Point -> [Point] -> [Point]
sortedByAngleWithXAxis (Point px py) = sortOn angle
        where angle (Point x y)
                | x == px && y == py = 0
                | x >= px            = atan2 (y - py) (x - px)
                | x < px             = pi + atan2 (y - py) (x - px)

-- This currently assumes the points are already sorted
scan :: [Point] -> [Point]
scan ps = let triples = inThrees ps
              triplesWithDirections = zip triples (map turnDirection triples)
                in
                if not (any (\z -> snd z == Clockwise) triplesWithDirections)
                   then ps
                   else let ccwTriples = map fst $ filter (\z -> snd z /= Clockwise) triplesWithDirections
                            in scan $ map midPoint ccwTriples

convexHull :: [Point] -> [Point]
convexHull ps = let point1 = initialPoint ps
                    sortedPoints = sortedByAngleWithXAxis point1 ps ++ [point1]
                in point1 : scan sortedPoints



-- Some test data

poly1 :: [Point]
poly1 = [Point 0 0, Point 3 1, Point 1 1, Point 3 3,
              Point 1 4, Point (-1) 2, Point (-2) 1]

poly2 :: [Point]
poly2 = [Point 0 0, Point 3 1, Point 2 2, Point 1.75 2, Point 2 4, Point 1 4]