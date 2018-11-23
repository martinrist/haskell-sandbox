{-# LANGUAGE RankNTypes #-}

module ProgrammingHaskell.Chapter16.Examples where

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
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x


------------------------------------
-- 16.11 - Ignoring Possibilities --
------------------------------------

-- Mapping over Maybe
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

-- The above are equivalent to
incMaybe :: Num a => Maybe a -> Maybe a
incMaybe = fmap (+1)

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe = fmap show

-- The fully-generic versions, which work for any Functor
liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show


-- Mapping over Either
incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left  e) = Left e

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left  e) = Left e

-- Equivalent to
incEither :: Num a => Either e a -> Either e a
incEither = fmap (+1)

showEither :: Show a => Either e a -> Either e String
showEither = fmap show


-------------------------------------------
-- 16.12 - A somewhat surprising functor --
-------------------------------------------

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant m) where
    fmap _ (Constant v) = Constant v


-------------------------------------------
-- 16.13 - More structure, more functors --
-------------------------------------------

data Wrap f a =
    Wrap (f a)
    deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)


----------------------------
-- 16.14 - The IO Functor --
----------------------------

getInt :: IO Int
getInt = fmap read getLine



-------------------------------------
-- 16.15 - Natural transformations --
-------------------------------------

-- This type is impossible because `f` and `g` are higher-kinded types and HKT's can't be arguments to the function type
--nat :: (f -> g) -> f a -> g a
--nat = undefined

-- If we use `RankNTypes` we can write the following type to represent a natural transformation from `f` to `g`:
type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- This won't work, because we're changing `a` - to do so, we'd need another instance (e.g. `Num`)
--maybeToList (Just a) = [a + 1]


-- Including the `a` as a type parameter will allow us to mess with the content
-- As a result, it's not a natural transformation
type Nat' f g a = f a -> g a

degenerateMtl :: Num a => Nat' Maybe [] a
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a + 1]