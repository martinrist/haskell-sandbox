module ProgrammingHaskell.Chapter16.Exercises.FunctorInstances where

import Test.QuickCheck
import GHC.Arr

-- Exercise 1 - `Bool`

-- This is already declared
-- data Bool = False | True

-- This can't be written, because `Bool` has kind `*` and we need kind `* -> *`
-- instance Functor Bool where
--     fmap = undefined


-- Exercise 2 - BoolAndSomethingElse

data BoolAndSomethingElse a =
    False' a | True' a
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
    arbitrary = oneof [False' <$> arbitrary,
                       True' <$> arbitrary]

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f (True'  a) = True'  (f a)


-- Exercise 3 - BoolAndMaybeSomethingElse

data BoolAndMaybeSomethingElse a =
    Falsish | Truish a
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
    arbitrary = oneof [pure Falsish,
                       Truish <$> arbitrary]

instance Functor BoolAndMaybeSomethingElse where
    fmap _ Falsish = Falsish
    fmap f (Truish a) = Truish (f a)


-- Exercise 4 - Mu

newtype Mu f = InF { outF :: f (Mu f) }

-- `Mu` has kind `(* -> *) -> *`, whereas we're expecting kind `* -> *`.
-- There's no way we can munge one into the other, so this won't work.

-- instance Functor Mu where
--      fmap = undefined


-- Exercise 5

data D = D (Array Word Word) Int Int

-- `D` has kind `*`, so we can't write a `Functor` instance for it.