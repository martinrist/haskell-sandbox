module ProgrammingHaskell.Chapter15.Exercises.Semigroup where

import           Test.QuickCheck         hiding ( Success
                                                , Failure
                                                )
import           Control.Applicative

-- This gives us an instance of `Show` for functions
-- which in turn allows us to derive an instance for `Combine`
import           Text.Show.Functions


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

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity b = Identity (a <> b)


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


-- Question 6 - BoolConj

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
    arbitrary = BoolConj <$> elements [True, False]

instance Semigroup BoolConj where
    (BoolConj a) <> (BoolConj b) = BoolConj (a && b)


-- Question 7 - BoolDisj

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
    arbitrary = BoolDisj <$> elements [True, False]

instance Semigroup BoolDisj where
    (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)


-- Question 8 - Or

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Fst a, Snd b]

instance Semigroup (Or a b) where
    Snd a <> _     = Snd a
    _     <> Snd b = Snd b
    Fst a <> Fst b = Fst b


-- Question 9 - Combine

newtype Combine a b = Combine { unCombine :: a -> b } deriving Show

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = Combine <$> arbitrary

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ \a -> f a <> g a


-- Question 10 - Comp

newtype Comp a = Comp { unComp :: a -> a } deriving Show

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = Comp <$> arbitrary

-- Notice that this instance doesn't require `Semigroup a`.  Since the function
-- inside `Comp` is of type `a -> a`, we can just use normal function composition
instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp $ \a -> f (g a)


-- Question 11 - Validation

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Failure a, Success b]

instance Semigroup a => Semigroup (Validation a b) where
    Success a <> _     = Success a
    _     <> Success b = Success b
    Failure a <> Failure b = Failure $ a <> b
