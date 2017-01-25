module Exercise10Comp where

import Data.Semigroup
import Data.Monoid (Sum)
import Test.QuickCheck

newtype Comp a =
    Comp { unComp :: a -> a }

instance Show (Comp a) where
    show _ = "comp"

instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp (f.g)

instance Arbitrary a => Arbitrary (Comp a) where
    arbitrary = undefined

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type CompInt = Comp Int
type CompAssoc = CompInt -> CompInt -> CompInt -> Bool

--main :: IO ()
--main =
--    quickCheck (semigroupAssoc :: CompAssoc)