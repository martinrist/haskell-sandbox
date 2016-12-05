-------------------------
-- Chapter 10 - Exercises
-------------------------
import Data.Time

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


-- Question 2
isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _            = False

getDbNumber :: DatabaseItem -> Integer
getDbNumber (DbNumber x) = x

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map getDbNumber . filter isDbNumber


-- Question 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr max (UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0)) . filterDbDate


-- Question 4
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber


-- Question 5
avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral sum) / (fromIntegral count)
    where sum = sumDb xs
          count = length (filterDbNumber xs)
