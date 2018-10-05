module Exercise9Combine where

import Data.Semigroup
import Data.Monoid (Sum)
import Test.QuickCheck
import Text.Show.Functions

newtype Combine a b =
    Combine { unCombine :: a -> b }
    deriving Show

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ \n -> f n <> g n

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        f <- arbitrary
        return (Combine f)

combineAssoc :: (Eq b, Semigroup b) => a -> Combine a b -> Combine a b -> Combine a b -> Bool
combineAssoc v a b c = unCombine (a <> (b <> c)) v == unCombine ((a <> b) <> c) v

instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = do
        a <- arbitrary
        return (Sum a)

type CombineIntSumInt = Combine Int (Sum Int)
type CombineAssoc = Int -> CombineIntSumInt -> CombineIntSumInt -> CombineIntSumInt -> Bool

main :: IO ()
main =
  quickCheck (combineAssoc :: CombineAssoc)