module Exercises where

---------------------------
-- Intermission - after 7.5
---------------------------

-- Question 1

k :: (a, b) -> a
k (x, y) = x


-- Question 2
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (x, y, z) (l, m, n) = ((x, l), (z, n))


---------------------------
-- Intermission - after 7.6
---------------------------

-- Question 1

functionC x y = if x > y then x else y

functionC' x y = case x > y of
                      True  -> x
                      False -> y


-- Question 2

ifEvenAdd2 n = if even n then n+2 else n
ifEvenAdd2' n = case even n of
                     True  -> n+2
                     False -> n

-- Question 3
nums :: (Num a, Ord a) => a -> a
nums x = case compare x 0 of
              LT -> -1
              GT -> 1
              EQ -> 0
