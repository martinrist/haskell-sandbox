{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ProgrammingHaskell.Chapter05.Exercises.DetermineTheType where

-- simple example
example = 1

-- Question 1
-- a :: Num a => a
a = (* 9) 6

-- b :: Num t => (t, [Char])
b = head [(0, "doge"), (1, "kitteh")]

-- c :: (Integer, [Char])
c = head [(0 :: Integer, "doge"), (1, "kitteh")]

-- d :: Bool
d =
    if False
        then True
        else False

-- e :: Int
e = length [1, 2, 3, 4, 5]

-- f :: Bool
f = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- Question 2
x = 5

y = x + 5

-- z :: Num a => a -> a
z v = v * 10

-- Question 3
x' = 5

y' = x' + 5

-- f' :: Fractional a => a
f' = 4 / y'

-- Question 5
x'' = "Julie"

y'' = " <3 "

z'' = "Haskell"

-- f'' :: [Char]
f'' = x'' ++ y'' ++ z''