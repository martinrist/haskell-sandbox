{-# LANGUAGE FlexibleInstances #-}

module ProgrammingHaskell.Chapter16.Exercises.MoreFunctorInstances where

import           Test.QuickCheck
import           Control.Applicative
import           Text.Show.Functions

-- Question 1 - Quant

data Quant a b =
    Finance
    | Desk a
    | Bloor b
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = oneof [return Finance,
                       Desk <$> arbitrary,
                       Bloor <$> arbitrary]

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)


-- Question 2 - K a b

data K a b = K a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (K a b) where
    arbitrary = K <$> arbitrary

instance Functor (K a) where
    fmap _ (K a) = K a


-- Question 3 - Flip

newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

newtype L a b = L a
    deriving (Eq, Show)

instance Arbitrary b => Arbitrary (Flip L a b) where
    arbitrary = Flip . L <$> arbitrary

instance Functor (Flip L a) where
    fmap f (Flip (L a)) = Flip $ L (f a)


-- Question 4 - EvilGoateeConst

data EvilGoateeConst a b =
    GoatyConst b
    deriving (Eq, Show)

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
    arbitrary = GoatyConst <$> arbitrary

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)


-- Question 5 - LiftItOut

data LiftItOut f a =
    LiftItOut (f a)
    deriving (Eq, Show)

instance (Applicative f, Arbitrary a) => Arbitrary (LiftItOut f a) where
    arbitrary = LiftItOut . pure <$> arbitrary

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut $ fmap f fa


-- Question 6 - Parappa

data Parappa f g a =
    DaWrappa (f a) (g a)
    deriving (Eq, Show)

instance (Applicative f, Applicative g, Arbitrary a) => Arbitrary (Parappa f g a) where
    arbitrary = do
        a1 <- arbitrary
        a2 <- arbitrary
        return $ DaWrappa (pure a1) (pure a2)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)


-- Question 7 - IgnoreOne

data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)
    deriving (Eq, Show)

instance (Applicative f, Applicative g, Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne f g a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ IgnoringSomething (pure a) (pure b)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)


-- Question 8 - Notorious

data Notorious g o a t =
    Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance (Applicative g, Arbitrary o, Arbitrary a, Arbitrary t) => Arbitrary (Notorious g o a t) where
    arbitrary = do
        o <- arbitrary
        a <- arbitrary
        t <- arbitrary
        return $ Notorious (pure o) (pure a) (pure t)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)


-- Question 9 - List

data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = oneof [return Nil,
                       liftA2 Cons arbitrary arbitrary]
                       -- do
                       --     a <- arbitrary
                       --     l <- arbitrary
                       --     return $ Cons a l]

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a rest) = Cons (f a) (fmap f rest)


-- Question 10 - GoatLord

data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a)
                (GoatLord a)
                (GoatLord a)
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (GoatLord a) where
    arbitrary = oneof [return NoGoat,
                       OneGoat <$> arbitrary,
                       liftA3 MoreGoats arbitrary arbitrary arbitrary]

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats gl1 gl2 gl3) = MoreGoats (fmap f gl1)
                                               (fmap f gl2)
                                               (fmap f gl3)


-- Question 11 - TalkToMe

data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)
    deriving Show

instance (Arbitrary a) => Arbitrary (TalkToMe a) where
    arbitrary = oneof [return Halt,
                       liftA2 Print arbitrary arbitrary,
                       Read <$> arbitrary]

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read fn) = Read $ fmap f fn
