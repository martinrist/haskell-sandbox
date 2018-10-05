module ProgrammingHaskell.Chapter05.Exercises.WriteTheFunction where

-- Question 1
i :: a -> a
i = id

-- Question 2
c :: a -> b -> a
c x _ = x

-- Question 3
c'' :: b -> a -> b
c'' b _ = b

-- Question 4
c' :: a -> b -> b
c' _ b = b

-- Question 5
r :: [a] -> [a]
r = reverse

-- Question 6
co :: (b -> c) -> (a -> b) -> a -> c
co f g = f . g

-- Question 7
a :: (a -> c) -> a -> a
a _ x = x