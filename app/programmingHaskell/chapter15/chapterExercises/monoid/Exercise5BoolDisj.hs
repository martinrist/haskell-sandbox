module Exercise5BoolDisj where

import Data.Semigroup
import Test.QuickCheck

newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj True <> _ = BoolDisj True
    BoolDisj False <> b  = b

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = elements [BoolDisj False, BoolDisj True]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolDisjId = BoolDisj -> Bool

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisjId)
    quickCheck (monoidRightIdentity :: BoolDisjId)