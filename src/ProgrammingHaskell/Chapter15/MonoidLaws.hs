module ProgrammingHaskell.Chapter15.MonoidLaws where

-- Associativity
type AssociativityProp a = a -> a -> a -> Bool

monoidAssoc :: (Eq m, Monoid m) => AssociativityProp m
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


-- A version that allows us to test semigroups of the form `Wrap { unWrap :: a -> b }`
-- and test associativity for the wrapped functions.  This gets around the fact
-- that we can't have an `Eq` instance for a function
wrappedFnAssoc :: (Monoid m, Eq b) => (m -> (a -> b)) -> a -> AssociativityProp m
wrappedFnAssoc unWrap v a b c = unWrap (a <> (b <> c)) v == unWrap ((a <> b) <> c) v

-- Identity
type IdentityProp a = a -> Bool

monoidLeftIdentity :: (Eq m, Monoid m) => IdentityProp m
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => IdentityProp m
monoidRightIdentity a = (a <> mempty) == a

wrappedFnLeftIdentity :: (Monoid m, Eq b) => (m -> (a -> b)) -> a -> IdentityProp m
wrappedFnLeftIdentity unWrap v a = unWrap (mempty <> a) v == unWrap a v

wrappedFnRightIdentity :: (Monoid m, Eq b) => (m -> (a -> b)) -> a -> IdentityProp m
wrappedFnRightIdentity unWrap v a = unWrap (a <> mempty) v == unWrap a v