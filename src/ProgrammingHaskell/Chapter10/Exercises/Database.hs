module ProgrammingHaskell.Chapter10.Exercises.Database where

import Data.Maybe (mapMaybe)
import Data.Time

data DatabaseItem
    = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

-- Question 1
-- Naive way of doing it, first filtering, then mapping
isDbDate :: DatabaseItem -> Bool
isDbDate i =
    case i of
        (DbDate _) -> True
        _ -> False

-- The problem is that this is a partial function
dbDateToUtcTime :: DatabaseItem -> UTCTime
dbDateToUtcTime (DbDate t) = t

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map dbDateToUtcTime . filter isDbDate

-- A better way, using Maybe:
dbItemToUtcTime :: DatabaseItem -> Maybe UTCTime
dbItemToUtcTime (DbDate t) = Just t
dbItemToUtcTime _ = Nothing

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' = mapMaybe dbItemToUtcTime

-- Question 2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = mapMaybe dbItemToInteger
  where
    dbItemToInteger (DbNumber i) = Just i
    dbItemToInteger _ = Nothing

-- Question 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- Another better version, using `Maybe` in case there are no time records
mostRecent' :: [DatabaseItem] -> Maybe UTCTime
mostRecent' = safeMax . filterDbDate'
  where
    safeMax [] = Nothing
    safeMax xs = Just $ maximum xs

-- Question 4
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- Question 5
avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / fromIntegral (length $ filterDbNumber db)