module ProgrammingHaskell.Chapter16.FunctorLaws where

import Test.QuickCheck.Function

-- Functor Identity

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = fmap id x == x


-- Functor Composition

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)
