module ProgrammingHaskell.Chapter15.Exercises.Semigroup where

import Test.QuickCheck
import Control.Applicative

-- Question 1 - Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
    arbitrary = return Trivial

instance Semigroup Trivial where
    _ <> _ = Trivial

-- Question 2 - Identity

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Semigroup (Identity a) where
    Identity a <> Identity b = Identity a

-- Question 3  - Two

data Two a b = Two a b
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

-- Question 4 - Three
data Three a b c = Three a b c
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

-- Question 5 - Four
data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')
