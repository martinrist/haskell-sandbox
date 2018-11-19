module ProgrammingHaskell.Chapter15.Exercises.Monoid where

import ProgrammingHaskell.Chapter15.Exercises.Semigroup

-- Question 1 -Trivial

instance Monoid Trivial where
    mempty = Trivial


