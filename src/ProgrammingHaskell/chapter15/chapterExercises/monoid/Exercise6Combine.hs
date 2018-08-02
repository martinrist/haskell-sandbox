module Exercise6Combine where

import Data.Semigroup
import Data.Monoid (Sum)
import Test.QuickCheck
import Text.Show.Functions

newtype Combine a b =
    Combine { unCombine :: a -> b }
    deriving Show

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ \n -> f n <> g n

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
    mempty = Combine (const mempty)
    mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        f <- arbitrary
        return (Combine f)

combineAssoc :: (Eq b, Semigroup b) => a -> Combine a b -> Combine a b -> Combine a b -> Bool
combineAssoc v a b c = unCombine (a <> (b <> c)) v == unCombine ((a <> b) <> c) v

combineLeftIdentity :: (Eq b, Semigroup b, Monoid b) => a -> Combine a b -> Bool
combineLeftIdentity v a = unCombine (mempty `mappend` a) v == unCombine a v

combineRightIdentity :: (Eq b, Semigroup b, Monoid b) => a -> Combine a b -> Bool
combineRightIdentity v a = unCombine (a `mappend` mempty) v == unCombine a v

instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = do
        a <- arbitrary
        return (Sum a)

type CombineIntSumInt = Combine Int (Sum Int)
type CombineAssoc = Int -> CombineIntSumInt -> CombineIntSumInt -> CombineIntSumInt -> Bool
type CombineId = Int -> CombineIntSumInt -> Bool

main :: IO ()
main = do
  quickCheck (combineAssoc :: CombineAssoc)
  quickCheck (combineLeftIdentity :: CombineId)
  quickCheck (combineRightIdentity :: CombineId)