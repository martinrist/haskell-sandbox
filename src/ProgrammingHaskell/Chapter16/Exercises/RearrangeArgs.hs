module ProgrammingHaskell.Chapter16.Exercises.RearrangeArgs where

import Test.QuickCheck
import Control.Applicative

-- Question 1 - Sum

data Sum b a =
    First a
  | Second b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = oneof [First <$> arbitrary,
                       Second <$> arbitrary]

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b


-- Question 2 - Company

data Company a c b =
    DeepBlue a c
  | Something b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Company a b c) where
      arbitrary = oneof [liftA2 DeepBlue arbitrary arbitrary,
                         fmap Something arbitrary]

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c


-- Question 3 - More

data More b a  =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (More a b) where
    arbitrary = oneof [liftA3 L arbitrary arbitrary arbitrary,
                       liftA3 R arbitrary arbitrary arbitrary
                       ]

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'