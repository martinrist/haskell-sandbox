module Exercise7BoolDisj where

import Data.Semigroup
import Test.QuickCheck

newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj True <> _ = BoolDisj True
    BoolDisj False <> b  = b

instance Arbitrary BoolDisj where
    arbitrary = elements [BoolDisj False, BoolDisj True]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

main :: IO ()
main =
    quickCheck (semigroupAssoc :: BoolDisjAssoc)