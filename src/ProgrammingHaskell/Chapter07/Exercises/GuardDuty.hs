module ProgrammingHaskell.Chapter07.Exercises.GuardDuty where

-- Question 1
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | otherwise = 'F'
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
  where
    y = x / 100

-- Question 2
avgGrade' :: (Fractional a, Ord a) => a -> Char
avgGrade' x
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.9 = 'A'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    | otherwise = 'F'
  where
    y = x / 100

-- Question 3
pal :: Eq a => [a] -> Bool
pal xs
    | xs == reverse xs = True
    | otherwise = False

-- Question 6
numbers :: (Num a, Ord a) => a -> Int
numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1
    | otherwise = 0