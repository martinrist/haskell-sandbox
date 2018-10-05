------------------------
-- Chapter 9 - Exercises
------------------------
import Data.Char


-- Chapter Exercises
--------------------
-- Question 2
filterUpper = filter isUpper

-- Question 3
capitaliseFirst :: String -> String
capitaliseFirst [] = []
capitaliseFirst (x:xs) = (toUpper x) : xs

-- Question 4
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = (toUpper x) : (capitalise xs)

-- Questions 5 & 6
getFirstAsCapital = toUpper . head

-- Standard functions
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

-- Question 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- Question 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny pred (x:xs) = pred x || (myAny pred xs)

-- Question 3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
    | x == y = True
    | otherwise = myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (== x)

-- Question 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ x : []

-- Question 5
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- Question 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- Question 7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- Question 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:xs)
    | f x maxRest == GT = x
    | otherwise = maxRest
  where
    maxRest = myMaximumBy f xs

-- Question 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x:xs)
    | f x minRest == LT = x
    | otherwise = minRest
  where
    minRest = myMinimumBy f xs
