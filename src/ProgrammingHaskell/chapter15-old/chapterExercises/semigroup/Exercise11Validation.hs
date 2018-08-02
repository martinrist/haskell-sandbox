module Exercise11Validation where

import Data.Semigroup
import Data.Monoid (Sum)
import Test.QuickCheck hiding (Failure, Success)
import Text.Show.Functions

data Validation a b =
    Failure a | Success b
    deriving (Eq, Show)

instance Semigroup (Validation a b) where
    Failure a <> _ = Failure a
    Success b <> _ = Success b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return (Failure a)),
                   (1, return (Success b))]


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

type ValStringInt = Validation String Int
type ValAssoc = ValStringInt -> ValStringInt -> ValStringInt -> Bool

main :: IO ()
main =
    quickCheck (semigroupAssoc :: ValAssoc)