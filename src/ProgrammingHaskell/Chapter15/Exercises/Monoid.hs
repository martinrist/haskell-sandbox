module ProgrammingHaskell.Chapter15.Exercises.Monoid where

import ProgrammingHaskell.Chapter15.Exercises.Semigroup
import Test.QuickCheck
import Text.Show.Functions

-- Question 1 -Trivial

instance Monoid Trivial where
    mempty = Trivial


-- Question 2 - Identity

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty


-- Question 3 - Two

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty


-- Question 4 - BoolConj

instance Monoid BoolConj where
    mempty = BoolConj True


-- Question 5 - BoolDisj

instance Monoid BoolDisj where
    mempty = BoolDisj False


-- Question 6 - Combine

instance Monoid b => Monoid (Combine a b) where
    mempty = Combine $ const mempty


-- Question 7 - Comp

instance Monoid (Comp a) where
   mempty = Comp id


-- Question 8 - Mem

newtype Mem s a = Mem { runMem :: s -> (a, s) } deriving Show

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
    arbitrary = Mem <$> arbitrary

instance Semigroup a => Semigroup (Mem s a) where
    (Mem f) <> (Mem g) = Mem $ \s -> let (a, s') = f s
                                         (a', s'') = g s' in
                                         (a <> a', s'')

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
