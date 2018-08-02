module Exercise6BoolConj where

import Data.Semigroup
import Test.QuickCheck

newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj False <> _ = BoolConj False
    BoolConj True <> b  = b

instance Arbitrary BoolConj where
    arbitrary = elements [BoolConj False, BoolConj True]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

main :: IO ()
main =
    quickCheck (semigroupAssoc :: BoolConjAssoc)