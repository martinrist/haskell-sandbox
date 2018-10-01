module ProgrammingHaskell.Chapter07.Exercises.CasePractice where

-- Question 1
functionC :: Ord t => t -> t -> t
functionC x y =
    if x > y
        then x
        else y

functionC' :: Ord t => t -> t -> t
functionC' x y =
    case x > y of
        True -> x
        False -> y

-- Question 2
ifEvenAdd2 :: Integral t => t -> t
ifEvenAdd2 n =
    if even n
        then n + 2
        else n

ifEvenAdd2' :: Integral t => t -> t
ifEvenAdd2' n =
    case even n of
        True -> n + 2
        False -> n

-- Question 3
nums :: (Num a, Ord a) => a -> Int
nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0