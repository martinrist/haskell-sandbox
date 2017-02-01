module Exercise3Two where

import Data.Semigroup
import Test.QuickCheck

data Two a b = Two a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b)  => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)  where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TwoStringIntList = Two String [Int]
type TwoAssoc = TwoStringIntList -> TwoStringIntList -> TwoStringIntList -> Bool

main :: IO ()
main =
    quickCheck (semigroupAssoc :: TwoAssoc)