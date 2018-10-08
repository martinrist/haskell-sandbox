module ProgrammingHaskell.Chapter10.Exercises.FunctionsAsFolds where

-- Question 1 - myOr
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- Question 2- myAny
myAny :: (a -> Bool) -> [a] -> Bool
myAny f =
    foldr
        (\a b ->
             if f a
                 then True
                 else b)
        False

-- Question 3 - myElem
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a _ -> a == x) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = foldr ((||) . (== x)) False

-- Question 4 - myReverse
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Question 5 - myMap
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- Question 6 - myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p =
    foldr
        (\x ys ->
             if p x
                 then x : [] ++ ys
                 else ys)
        []

-- Question 7 - squish
squish :: [[a]] -> [a]
squish = foldr (++) []

-- Question 8 - squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- Question 9 - squishAgain
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- Question 10 - myMaximumBy
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs =
    foldr
        (\x y ->
             if f x y == GT
                 then x
                 else y)
        (head xs)
        xs

-- Question 11 - myMinimumBy
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs =
    foldr
        (\x y ->
             if f x y == LT
                 then x
                 else y)
        (head xs)
        xs
