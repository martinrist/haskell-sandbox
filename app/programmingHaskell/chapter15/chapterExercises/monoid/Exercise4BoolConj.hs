module Exercise4BoolConj where

import Data.Semigroup
import Test.QuickCheck

newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj False <> _ = BoolConj False
    BoolConj True <> b  = b

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = elements [BoolConj False, BoolConj True]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolConjId = BoolConj -> Bool

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConjId)
    quickCheck (monoidRightIdentity :: BoolConjId)