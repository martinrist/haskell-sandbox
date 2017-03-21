import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import GHC.Generics

--------------------------
-- Chapter 17 Exercises --
--------------------------

-- Exercise 1 - Pair

data Pair a =
    Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Eq a => EqProp (Pair a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Pair x y

testPairFunctor :: IO ()
testPairFunctor = quickBatch (functor (undefined :: Pair (String, String, Int)))

instance Applicative Pair where
    pure a = Pair a a
    Pair f1 f2 <*> Pair v1 v2 = Pair (f1 v1) (f2 v2)

testPairApplicative :: IO ()
testPairApplicative = quickBatch (applicative (undefined :: Pair (String, String, Int)))


-- Exercise 2 - Two

data Two a b =
    Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

testTwoFunctor :: IO ()
testTwoFunctor = quickBatch (functor (undefined :: Two Int (String, String, Int)))

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x
    Two a b <*> Two a' b' = Two (a `mappend` a') (b b')

testTwoApplicative :: IO ()
testTwoApplicative = quickBatch (applicative (undefined :: Two String (String, String, Int)))
