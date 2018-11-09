module ProgrammingHaskell.Chapter14.Exercises.Generators where

import Test.QuickCheck

-- Exercise 1 - `Fool1` - equal probabilities of `Fulse1` and `Frue1`

data Fool1 = Fulse1 | Frue1 deriving (Eq, Show)

instance Arbitrary Fool1 where
    arbitrary = elements [Fulse1, Frue1]

-- Exercise 2 - `Fool2` - 2/3 chance of `Fulse2`, 1/3 chance of `Frue2`

data Fool2 = Fulse2 | Frue2 deriving (Eq, Show)

instance Arbitrary Fool2 where
    arbitrary = frequency [(2, pure Fulse2),
                           (1, pure Frue2)]
