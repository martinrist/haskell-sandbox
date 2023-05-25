{-# OPTIONS_GHC -Wno-missing-signatures #-}
module EffectiveHaskell.Chapter01.Exercises where

factorial n
    | n == 1 = 1
    | otherwise = n * factorial (n - 1)

fibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibonacci (n - 2) + fibonacci (n - 1)

uncurry' f (a, b) = f a b

curry' f a b = f (a, b)
