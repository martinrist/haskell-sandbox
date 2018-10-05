{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ProgrammingHaskell.Chapter02.Examples where

----------------------------
-- Chapter 2 - Hello Haskell
----------------------------
---------------------
-- 2.12 - Let & Where
---------------------
printInc1 n = print plusTwo
  where
    plusTwo = n + 2

printInc2 n =
    let plusTwo = n + 2
     in print plusTwo

-- Either of the above forms can be rewritten as a lambda
printInc2' n = (\plusTwo -> print plusTwo) (n + 2)

-- Intermission: Exercises after 2.12
-- Rewriting `let` expressions as `where` expressions or lambdas
exercise_2_12_1 =
    let x = 3
        y = 1000
     in x * 3 + y

exercise_2_12_1' = x * 3 + y
  where
    x = 3
    y = 1000

exercise_2_12_1'' = (\x -> (\y -> x * 3 + y)) 3 1000

exercise_2_12_2 =
    let y = 10
        x = 10 * 5 + y
     in x * 5

exercise_2_12_2' = x * 5
  where
    y = 10
    x = 10 * 5 + y

exercise_2_12_2'' = (\y -> (\x -> x * 5) (10 * 5 + y)) 10

exercise_2_12_3 =
    let x = 7
        y = negate x
        z = y * 10
     in z / x + y

exercise_2_12_3' = z / x + y
  where
    x = 7
    y = negate x
    z = y * 10

exercise_2_12_3'' = (\x -> (\y -> (\z -> z / x + y) (y * 10)) (negate x)) 7
