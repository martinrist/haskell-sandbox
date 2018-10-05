module ProgrammingHaskell.Chapter09.Exercises.EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool = enumFromTo'

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = enumFromTo'

eftInt :: Int -> Int -> [Int]
eftInt = enumFromTo'

eftChar :: Char -> Char -> String
eftChar = enumFromTo'

enumFromTo' :: (Eq a, Ord a, Enum a) => a -> a -> [a]
enumFromTo' a b
    | a == b = [a]
    | a > b = []
    | otherwise = a : enumFromTo' (succ a) b
