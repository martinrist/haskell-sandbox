{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Arr

-----------------------
-- Chapter Exercises --
-----------------------
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

-- Can a valid Functor be written?

-- Exercise 1 - Bool

-- Can't write a `Functor` instance for `Bool` because `Bool` has kind *
--instance Functor Bool where
--    fmap _ False = False
--    fmap _ True  = True


-- Exercise 2 - BoolAndSomethingElse
data BoolAndSomethingElse a =
    False' a
  | True' a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
    arbitrary = do
        a <- arbitrary
        oneof [return $ False' a, return $ True' a]

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f (True' a)  = True' (f a)

exercise2 :: IO ()
exercise2 = do
    quickCheck $ \x -> functorIdentity (x :: BoolAndSomethingElse Int)
    quickCheck (functorCompose :: BoolAndSomethingElse Int -> IntToInt -> IntToInt -> Bool)

-- Exercise 3 - BoolAndMaybeSomethingElse
data BoolAndMaybeSomethingElse a =
    Falsish
  | Truish a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
    arbitrary = do
        a <- arbitrary
        oneof [return Falsish, return $ Truish a]

instance Functor BoolAndMaybeSomethingElse where
    fmap _ Falsish    = Falsish
    fmap f (Truish a) = Truish (f a)

exercise3 :: IO ()
exercise3 = do
    quickCheck $ \x -> functorIdentity (x :: BoolAndMaybeSomethingElse Int)
    quickCheck (functorCompose :: BoolAndMaybeSomethingElse Int -> IntToInt -> IntToInt -> Bool)


-- Exercise 4 - Mu
newtype Mu f = InF { outF :: f (Mu f) }

-- Can't do this because `Mu` has kind (* -> *) -> *
--instance Functor Mu  where
--    fmap = undefined


-- Exercise 5 - D
data D = D (Array Word Word) Int Int

-- D has kind *, so we can't write a Functor instance for it
--instance Functor D where
--    fmap = undefined




-- Rearranging type arguments to make the Functor instance work

-- Exercise 1 - Sum a b
--data Sum a b =
data Sum b a =
    First a
  | Second b

instance Functor (Sum e) where
    fmap f (First a)  = First (f a)
    fmap f (Second b) = Second b

-- Exercise 2 - Company a b c
--data Company a b c =
data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c


-- Exercise 3 - More a b
--data More a b =
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'


-- Writing Functor instances

-- Exercise 1 - Quant a b
data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return Finance, return $ Desk a, return $ Bloor b]

instance Functor (Quant a) where
    fmap _ Finance   = Finance
    fmap _ (Desk a)  = Desk a
    fmap f (Bloor b) = Bloor $ f b

exercise1' :: IO ()
exercise1' = do
    quickCheck $ \x -> functorIdentity (x :: Quant String Int)
    quickCheck (functorCompose :: Quant String Int -> IntToInt -> IntToInt -> Bool)


-- Exercise 2' - K
data K a b = K a
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (K a b) where
    arbitrary = do
        a <- arbitrary
        return (K a)

instance Functor (K a) where
    fmap _ (K a) = K a

exercise2' :: IO ()
exercise2' = do
    quickCheck $ \x -> functorIdentity (x :: K String Int)
    quickCheck (functorCompose :: K String Int -> IntToInt -> IntToInt -> Bool)


-- Exercise 3 - Flip

newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

newtype K' a b = K' a
    deriving (Eq, Show)

instance Functor (Flip K' a) where
    fmap f (Flip (K' a)) = Flip $ K' (f a)


-- Exercise 4 - EvilGoateeConst

data EvilGoateeConst a b =
    GoatyConst b
    deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst a) = GoatyConst (f a)