module ProgrammingHaskell.Chapter15.Exercises.First where

import ProgrammingHaskell.Chapter15.Exercises.Optional
import Test.QuickCheck
import Control.Monad

newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Semigroup (First' a) where
    First' Nada <> y           = y
    x           <> First' Nada = x
    x           <> y           = x

instance Monoid (First' a) where
    mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = frequency [(1, return (First' Nada)),
                           (3, First' . Only <$> arbitrary)]
