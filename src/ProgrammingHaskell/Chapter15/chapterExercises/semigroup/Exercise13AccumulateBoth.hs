module Exercise13AccumulateBoth where

import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)
import Text.Show.Functions

data Validation a b =
    Failure a | Success b
    deriving (Eq, Show)

newtype AccumulateBoth a b =
    AccumulateBoth (Validation a b)
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    AccumulateBoth (Failure a) <> AccumulateBoth (Failure a') = AccumulateBoth $ Failure (a <> a')
    AccumulateBoth (Success b) <> AccumulateBoth (Success b') = AccumulateBoth $ Success (b <> b')
    AccumulateBoth (Failure a) <> _                           = AccumulateBoth $ Failure a
    _                          <> AccumulateBoth (Failure a') = AccumulateBoth $ Failure a'


instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ AccumulateBoth $ Failure a,
               return $ AccumulateBoth $ Success b]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

type ABValStringListInt = AccumulateBoth String [Int]
type ABValAssoc = ABValStringListInt -> ABValStringListInt -> ABValStringListInt -> Bool

main :: IO ()
main =
    quickCheck (semigroupAssoc :: ABValAssoc)