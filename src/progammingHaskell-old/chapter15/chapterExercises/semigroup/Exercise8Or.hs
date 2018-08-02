module Exercise8Or where

import Data.Semigroup
import Test.QuickCheck

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
    Snd x <> _ = Snd x
    Fst x <> y = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Fst a,
               return $ Snd b]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type OrStringIntList = Or String [Int]
type OrAssoc = OrStringIntList -> OrStringIntList -> OrStringIntList -> Bool

main :: IO ()
main =
    quickCheck (semigroupAssoc :: OrAssoc)