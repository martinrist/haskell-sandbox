module ProgrammingHaskell.Chapter12.Exercises.IterateAndUnfoldr where

-- Question 1
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- Question 2
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z =
    case f z of
        Nothing -> []
        Just (a, b) -> a : myUnfoldr f b

-- Question 3
myIterate' :: (a -> a) -> a -> [a]
myIterate' f = myUnfoldr (\a -> Just (a, f a))