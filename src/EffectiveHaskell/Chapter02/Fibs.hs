{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module EffectiveHaskell.Chapter02.Fibs where

fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n - 1) + fib (n - 2)

fibs = map fib [0..]

smallFibs = takeWhile (< 100) fibs

fibs firstFib secondFib =
    let nextFib = firstFib + secondFib
    in firstFib : fibs secondFib nextFib
