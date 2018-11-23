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


-- Exercise 5 - LiftItOut

data LiftItOut f a =
    LiftItOut (f a)
    deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut x) = LiftItOut (fmap g x)


-- Exercise 6 - Parappa
data Parappa f g a =
    DataWrappa (f a) (g a)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap fn (DataWrappa x y) = DataWrappa (fmap fn x) (fmap fn y)


-- Exercise 7 - IgnoreOne
data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)
    deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap fn (IgnoringSomething x y) = IgnoringSomething x (fmap fn y)


-- Exercise 8 - Notorious
data Notorious g o a t =
    Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap fn (Notorious x y z) = Notorious x y (fmap fn z)


-- Exercise 9 - List
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
    fmap f Nil         = Nil
    fmap f (Cons x y)  = Cons (f x) (fmap f y)


-- Exercise 10 - GoatLord
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat             = NoGoat
    fmap f (OneGoat x)        = OneGoat (f x)
    fmap f (MoreGoats x y z ) = MoreGoats (fmap f x) (fmap f y) (fmap f z)


-- Exercise 11 - TalkToMe
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt        = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g)    = Read $ \x -> f (g x)