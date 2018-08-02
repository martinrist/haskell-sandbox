module Exercise1Trivial where

import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivialId = Trivial -> Bool

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: TrivialId)
    quickCheck (monoidRightIdentity :: TrivialId)