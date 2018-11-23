module ProgrammingHaskell.Chapter15.MonoidLaws where

-- Associativity
type AssociativityProp a = a -> a -> a -> Bool

monoidAssoc :: (Eq m, Monoid m) => AssociativityProp m
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Identity
type IdentityProp a = a -> Bool

monoidLeftIdentity :: (Eq m, Monoid m) => IdentityProp m
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => IdentityProp m
monoidRightIdentity a = (a <> mempty) == a
