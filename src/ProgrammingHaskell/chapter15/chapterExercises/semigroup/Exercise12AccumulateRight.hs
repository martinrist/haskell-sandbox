module Exercise12AccumulateRight where

import Data.Semigroup
import Data.Monoid (Sum)
import Test.QuickCheck hiding (Failure, Success)
import Text.Show.Functions

data Validation a b =
    Failure a | Success b
    deriving (Eq, Show)

newtype AccumulateRight a b =
    AccumulateRight (Validation a b)
    deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    AccumulateRight (Failure a) <> _                            = AccumulateRight $ Failure a
    AccumulateRight (Success b) <> AccumulateRight (Failure a)  = AccumulateRight $ Failure a
    AccumulateRight (Success b) <> AccumulateRight (Success b') = AccumulateRight $ Success (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ AccumulateRight $ Failure a,
               return $ AccumulateRight $ Success b]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

type ARValStringListInt = AccumulateRight String [Int]
type ARValAssoc = ARValStringListInt -> ARValStringListInt -> ARValStringListInt -> Bool

main :: IO ()
main =
    quickCheck (semigroupAssoc :: ARValAssoc)