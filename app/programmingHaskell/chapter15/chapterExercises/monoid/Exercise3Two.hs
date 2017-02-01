module Exercise3Two where

import Data.Semigroup
import Test.QuickCheck

data Two a b = Two a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)  where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

type TwoStringIntList = Two String [Int]
type TwoAssoc = TwoStringIntList -> TwoStringIntList -> TwoStringIntList -> Bool
type TwoId = TwoStringIntList -> Bool

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: TwoId)
    quickCheck (monoidRightIdentity :: TwoId)