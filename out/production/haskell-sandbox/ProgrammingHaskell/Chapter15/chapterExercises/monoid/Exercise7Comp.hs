module Exercise7Comp where

import Data.Semigroup
import Data.Monoid (Sum)
import Test.QuickCheck
import Text.Show.Functions

newtype Comp a =
    Comp { unComp :: (a -> a) }
    deriving Show

instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp (f.g)

instance Monoid (Comp a) where
    mempty = Comp id
    mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = do
        f <- arbitrary
        return (Comp f)

compAssoc :: (Eq a, Semigroup a) => a -> Comp a -> Comp a -> Comp a -> Bool
compAssoc v a b c = unComp (a <> (b <> c)) v == unComp ((a <> b) <> c) v

compLeftIdentity :: (Eq a, Semigroup a, Monoid a) => a -> Comp a -> Bool
compLeftIdentity v a = unComp (mempty `mappend` a) v == unComp a v

compRightIdentity :: (Eq a, Semigroup a, Monoid a) => a -> Comp a -> Bool
compRightIdentity v a = unComp (a `mappend` mempty) v == unComp a v

type CompListInt = Comp [Int]
type CompAssoc = [Int] -> CompListInt -> CompListInt -> CompListInt -> Bool
type CompId = [Int] -> CompListInt -> Bool

main :: IO ()
main = do
    quickCheck (compAssoc :: CompAssoc)
    quickCheck (compLeftIdentity :: CompId)
    quickCheck (compRightIdentity :: CompId)