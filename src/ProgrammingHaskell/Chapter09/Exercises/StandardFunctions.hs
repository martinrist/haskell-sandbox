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