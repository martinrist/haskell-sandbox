module Exercise10Comp where

import Data.Semigroup
import Data.Monoid (Sum)
import Test.QuickCheck
import Text.Show.Functions

newtype Comp a =
    Comp { unComp :: a -> a }
    deriving Show

instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp (f.g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = do
        f <- arbitrary
        return (Comp f)

compAssoc :: (Eq a, Semigroup a) => a -> Comp a -> Comp a -> Comp a -> Bool
compAssoc v a b c = unComp (a <> (b <> c)) v == unComp ((a <> b) <> c) v

type CompListInt = Comp [Int]
type CompAssoc = [Int] -> CompListInt -> CompListInt -> CompListInt -> Bool

main :: IO ()
main =
    quickCheck (compAssoc :: CompAssoc)