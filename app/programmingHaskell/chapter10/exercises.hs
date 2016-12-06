-------------------------
-- Chapter 10 - Exercises
-------------------------
import Data.Time
import Data.Maybe (mapMaybe)

-- Exercises: Understanding Folds
---------------------------------

-- Question 1
-------------

-- foldr (*) 1 [1..5]
-- = 120
-- = foldl (flip (*)) 1 [1..5]


-- Question 2
-------------

--   foldl (flip (*)) 1 [1..3]
-- = foldl (flip (*)) 1 [1, 2, 3]
-- = foldl (flip (*)) (1 (flip (*)) 1) [2, 3]
-- = foldl (flip (*)) ((1 (flip (*)) 1) (flip (*)) 2) [3]
-- = foldl (flip (*)) (((1 (flip (*)) 1) (flip (*)) 2) (flip (*)) 3) []
-- =                  (((1 (flip (*)) 1) (flip (*)) 2) (flip (*)) 3)
-- =                  (((1 * 1) * 2) * 3)
-- =                  6



-- Exercises : Database Processing
----------------------------------

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [ DbDate (UTCTime (fromGregorian 1911 5 1)
                      (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbNumber 10
    , DbDate (UTCTime (fromGregorian 1921 5 1)
                      (secondsToDiffTime 34123))
             ]


-- Question 1
isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _          = False

getDbDate :: DatabaseItem -> UTCTime
getDbDate (DbDate x) = x

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map getDbDate . filter isDbDate

-- Better version, using Maybe and mapMaybe
maybeDbDate :: DatabaseItem -> Maybe UTCTime
maybeDbDate (DbDate x) = Just x
maybeDbDate _          = Nothing

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' = mapMaybe maybeDbDate



-- Question 2
isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _            = False

getDbNumber :: DatabaseItem -> Integer
getDbNumber (DbNumber x) = x

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map getDbNumber . filter isDbNumber

-- Better version, using Maybe and mapMaybea
maybeDbNumber :: DatabaseItem -> Maybe Integer
maybeDbNumber (DbNumber x) = Just x
maybeDbNumber _ = Nothing

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' = mapMaybe maybeDbNumber


-- Question 3

epochTime :: UTCTime
epochTime = (UTCTime (fromGregorian 1900 1 1)
                 (secondsToDiffTime 0))

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr max epochTime . filterDbDate'


-- Question 4
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber'


-- Question 5
avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral sum) / (fromIntegral count)
    where sum = sumDb xs
          count = length (filterDbNumber' xs)



-- Chapter Exercises
--------------------

-- Warm-up and review
---------------------

-- Question 1
stops = "pbtdkg"
vowels = "aeiou"

stopVowelStops = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

stopVowelStops' = [('p', v, s2) | v <- vowels, s2 <- stops]

-- Question 2
seekritFunc x = div (sum (map length (words x)))
                    (length (words x))

-- Question 3
seekritFunc' x = (/) (fromIntegral (sum (map length (words x))))
                    (fromIntegral (length (words x)))


-- Rewriting functions using folds
----------------------------------

-- Example - myAnd
-- Direct recursion
myAnd1 :: [Bool] -> Bool
myAnd1 []     = True
myAnd1 (x:xs) = if x == False
                then False
                else myAnd1 xs

-- Direct recursion, using (&&)
myAnd2 :: [Bool] -> Bool
myAnd2 []     = True
myAnd2 (x:xs) = x && myAnd2 xs

-- fold, not point-free in the folding function
myAnd3 :: [Bool] -> Bool
myAnd3 = foldr (\a b ->
                    if a == False
                    then False
                    else b) True

-- fold, fully point-free
myAnd4 :: [Bool] -> Bool
myAnd4 = foldr (&&) True


-- Question 2 - myOr
myOr :: [Bool] -> Bool
myOr = foldr (||) False


-- Question 3 - myElem
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr
            (\a _ -> a == x)
            False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = foldr ((||).(== x)) False


-- Question 4 - myReverse
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


-- Question 5 - myMap
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:).f) []


-- Question 6 - myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr (\x ys -> if pred x
                                then x:[] ++ ys
                                else ys)
                      []


-- Question 7 - squish
squish :: [[a]] -> [a]
squish = foldr (++) []


-- Question 8 - squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++).f) []


-- Question 9 - squishAgain
squishAgain = squishMap id


-- Question 10 - myMaximumBy
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> if f x y == GT
                                  then x
                                  else y)
                         (head xs) xs


-- Question 11 - myMinimumBy
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x y -> if f x y == LT
                                  then x
                                  else y)
                         (head xs) xs
