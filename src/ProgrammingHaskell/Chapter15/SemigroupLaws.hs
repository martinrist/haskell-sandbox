module ProgrammingHaskell.Chapter15.SemigroupLaws where

-- Associativity
type SemigroupAssoc m = m -> m -> m -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => SemigroupAssoc m
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)