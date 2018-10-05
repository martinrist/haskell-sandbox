{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module ProgrammingHaskell.Chapter08.Examples where

------------------
-- 8.2 - Factorial
------------------
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

---------------
-- 8.3 - Bottom
---------------
f :: Bool -> Int
f True = error "blah"
f False = 0

partialF :: Bool -> Int
partialF False = 0

maybeF :: Bool -> Maybe Int
maybeF False = Just 0
maybeF _ = Nothing

--------------------------
-- 8.4 - Fibonacci Numbers
--------------------------
fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

---------------------------------------
-- 8.5 - Integral division from scratch
---------------------------------------
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)