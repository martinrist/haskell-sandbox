module ProgrammingHaskell.Chapter15.SemigroupLaws where

-- Associativity
type AssociativityProp m = m -> m -> m -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => AssociativityProp m
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- A version that allows us to test semigroups of the form `Wrap { unWrap :: a -> b }`
-- and test associativity for the wrapped functions.  This gets around the fact
-- that we can't have an `Eq` instance for a function
wrappedFnAssoc :: (Semigroup s, Eq b) => (s -> (a -> b)) -> a -> AssociativityProp s
wrappedFnAssoc unWrap v a b c = unWrap (a <> (b <> c)) v == unWrap ((a <> b) <> c) v