module ProgrammingHaskell.Chapter09.Exercises.Zip where

-- Question 1
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _          = []
myZip _ []          = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- Question 2
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _          = []
myZipWith _ _ []          = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
