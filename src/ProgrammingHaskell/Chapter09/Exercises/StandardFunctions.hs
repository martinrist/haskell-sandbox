module ProgrammingHaskell.Chapter09.Exercises.StandardFunctions where

-- Question 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
    if x == True
        then True
        else myOr xs

-- Question 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) =
    if f x == True
        then True
        else myAny f xs

-- Question 3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) =
    if x == y
        then True
        else myElem x ys

-- Question 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

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
myMaximumBy _ [] = error "Empty list"
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) =
    let maxRest = myMaximumBy f xs
     in case f x maxRest of
            LT -> maxRest
            EQ -> x
            GT -> x

-- Question 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "Empty list"
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) =
    let minRest = myMinimumBy f xs
     in case f x minRest of
            LT -> x
            EQ -> minRest
            GT -> minRest

-- Question 10
myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare