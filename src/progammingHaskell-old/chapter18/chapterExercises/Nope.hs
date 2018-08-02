module Nope where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Question 1 - Nope

data Nope a =
    NopeDotJpg
    deriving (Eq, Show)

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq


instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure a = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    return = pure
    _ >>= f = NopeDotJpg

main :: IO ()
main = do
    let trigger = undefined :: Nope (Int, String, Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger