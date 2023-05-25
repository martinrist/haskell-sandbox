{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module EffectiveHaskell.Chapter02.Examples where

import Prelude hiding (foldl, foldr)

words = ["foo", "bar", "baz", "fizz", "buzz"]

listIsEmpty list =
    if null list
        then putStrLn "This list is empty"
        else putStrLn ("The first element of this list is: " <> show (head list))

countdown n =
    if n <= 0
        then []
        else n : countdown (n - 1)

factors num =
    factors' num 2
  where
    factors' num fact
        | num == 1 = []
        | (num `rem` fact) == 0 = fact : factors' (num `div` fact) fact
        | otherwise = factors' num (fact + 1)

-- This is our custom version of `Prelude.foldl`
foldl func carryValue lst =
    if null lst
        then carryValue
        else foldl func (func carryValue (head lst)) (tail lst)

-- This is our custom version of `Prelude.foldr`
foldr func carryValue lst =
    if null lst
        then carryValue
        else func (head lst) $ foldr func carryValue (tail lst)

isBalanced str =
    0 == foldl checkBalance 0 str
  where
    checkBalance count letter
        | letter == '(' = count + 1
        | letter == ')' = count - 1
        | otherwise = count

divide = (/)
infixr 9 `divide`

-- The l in `foldl` stands for left-associative
-- In a left fold, the initial value is applied first, at the left-hand side of the unrolled expression
-- In a left fold the accumulator value is the first (left) argument of the function you pass in
-- The r in foldr stands for right-associative
-- In a right fold, the initial value is applied last, at the right hand side of an unrolled expression
-- In a right fold, the accumulator is the second (right) argument of the function that you pass in”

numbersStartingAt n
    | n == 100 = undefined
    | otherwise = n : numbersStartingAt (n + 1)

radsToDegrees :: Float -> Int
radsToDegrees radians =
    let degrees = cycle [0..359]
        converted = truncate $ (radians * 360) / (2 * pi)
    in degrees !! converted

epicCycle inputList =
    cycleHelper inputList
    where
        cycleHelper [] = epicCycle inputList
        cycleHelper (x: xs) = x : cycleHelper xs

moreEpicCycle inputList =
    inputList <> moreEpicCycle inputList

main = putStrLn "Test"
