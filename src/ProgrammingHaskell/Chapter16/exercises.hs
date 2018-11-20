------------------------------
-- Exercises: Heavy Lifting --
------------------------------
import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g  . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)


-------------------------
-- Exercise : Possibly --
-------------------------

data Possibly a =
      LolNope
    | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope     = LolNope
    fmap f (Yeppers x) = Yeppers (f x)

instance Arbitrary a => Arbitrary (Possibly a) where
    arbitrary = do
        a <- arbitrary
        elements [LolNope, Yeppers a]

exercisePossibly :: IO ()
exercisePossibly = do
    quickCheck $ \x -> functorIdentity (x :: Possibly Int)
    quickCheck (functorCompose' :: Possibly Int -> Fun Int Int -> Fun Int Int -> Bool)


-----------------------
-- Exercise : Either --
-----------------------

data Sum a b =
      First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ First a, return $ Second b]

exerciseSum :: IO ()
exerciseSum = do
    quickCheck $ \x -> functorIdentity (x :: Sum String Int)
    quickCheck (functorCompose' :: Sum String Int -> Fun Int Int -> Fun Int Int -> Bool)


