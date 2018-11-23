module ProgrammingHaskell.Chapter15.SemigroupLaws where

-- Associativity
type AssociativityProp m = m -> m -> m -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => AssociativityProp m
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)