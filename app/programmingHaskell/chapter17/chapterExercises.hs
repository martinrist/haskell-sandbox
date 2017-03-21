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


-- Exercise 3 - Three

data Three a b c =
    Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

testThreeFunctor :: IO ()
testThreeFunctor = quickBatch (functor (undefined :: Three Int Int (String, String, Int)))

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    Three a b c <*> Three a' b' c' = Three (a `mappend` a') (b `mappend` b') (c c')

testThreeApplicative :: IO ()
testThreeApplicative = quickBatch (applicative (undefined :: Three String String (String, String, Int)))


-- Exercise 4 - Three'

data Three' a b =
    Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a ) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

testThree'Functor :: IO ()
testThree'Functor = quickBatch (functor (undefined :: Three' Int (String, String, Int)))

instance Monoid a => Applicative (Three' a) where
    pure x = Three' mempty x x
    Three' a b c <*> Three' a' b' c' = Three' (a `mappend` a') (b b') (c c')

testThree'Applicative :: IO ()
testThree'Applicative = quickBatch (applicative (undefined :: Three' String (String, String, Int)))


-- Exercise 5 - Four

data Four a b c d =
    Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four w x y z) = Four w x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

testFourFunctor :: IO ()
testFourFunctor = quickBatch (functor (undefined :: Four Int String String (String, String, Int)))

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure x = Four mempty mempty mempty x
    Four a b c d <*> Four a' b' c' d' = Four (a `mappend` a')
                                             (b `mappend` b')
                                             (c `mappend` c')
                                             (d d')

testFourApplicative :: IO ()
testFourApplicative = quickBatch (applicative (undefined :: Four String String String (String, String, Int)))


-- Exercise 5 - Four'

data Four' a b =
    Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' w x y z) = Four' w x y (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

testFour'Functor :: IO ()
testFour'Functor = quickBatch (functor (undefined :: Four' Int (String, String, Int)))

instance Monoid a => Applicative (Four' a) where
    pure x = Four' mempty mempty mempty x
    Four' a b c d <*> Four' a' b' c' d' = Four' (a `mappend` a')
                                                (b `mappend` b')
                                                (c `mappend` c')
                                                (d d')

testFour'Applicative :: IO ()
testFour'Applicative = quickBatch (applicative (undefined :: Four' String (String, String, Int)))



-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
