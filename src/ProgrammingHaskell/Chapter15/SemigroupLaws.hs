module ProgrammingHaskell.Chapter15.SemigroupLaws where

-- Associativity
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type SemigroupAssoc m = m -> m -> m -> Bool