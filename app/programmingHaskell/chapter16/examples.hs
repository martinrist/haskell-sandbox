{-# LANGUAGE TypeSynonymInstances #-}

---------------------------
-- Chapter 16 - Examples --
---------------------------

-- 16.6 - Good and bad Functors

data WhoCares a =
    ItDoesnt
    | Matter a
    | WhatThisIsCalled
    deriving (Eq, Show)

-- This ia a good example
instance Functor WhoCares where
    fmap _ ItDoesnt         = ItDoesnt
    fmap _ WhatThisIsCalled = WhatThisIsCalled
    fmap f (Matter a)       = Matter (f a)

-- This is a bad example that breaks the identity law
newtype WhoCaresBad a = WhoCaresBad (WhoCares a)
    deriving (Eq, Show)

instance Functor WhoCaresBad where
    fmap _ (WhoCaresBad ItDoesnt)         = WhoCaresBad WhatThisIsCalled
    fmap _ (WhoCaresBad WhatThisIsCalled) = WhoCaresBad ItDoesnt
    fmap f (WhoCaresBad (Matter a))       = WhoCaresBad $ Matter (f a)


-- This is a broken example which messes with the structure (by incrementing the `n`)
data CountingBad a =
    Heisenberg Int a
    deriving (Eq, Show)

instance Functor CountingBad where
    fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)


-- 16.7 - Commonly used functors

lms :: [Maybe String]
lms = [Just "Ave", Nothing, Just "woohoo"]



-- 16.8 - Transforming unapplied type arguments

data Two a b =
    Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

data Or a b = 
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Or a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second $ f b


-- 16.9 - QuickChecking Functor instances

functorIdentity :: (Functor f, Eq (f a)) =>
                             f a
                          -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                             (a -> b)
                          -> (b -> c)
                          -> f a
                          -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)