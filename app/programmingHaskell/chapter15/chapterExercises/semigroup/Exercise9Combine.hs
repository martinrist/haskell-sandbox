module Exercise9Combine where

import Data.Semigroup
import Data.Monoid (Sum)
import Test.QuickCheck

newtype Combine a b =
    Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ \n -> f n <> g n

instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        -- Is this where I need to use Coarbitrary I wonder?
        return (Combine a b)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = do
        a <- arbitrary
        return (Sum a)

type CombineIntSumInt = Combine Int (Sum Int)
type CombineAssoc = CombineIntSumInt -> CombineIntSumInt -> CombineIntSumInt -> Bool

main :: IO ()
main =
    quickCheck (semigroupAssoc :: CombineAssoc)