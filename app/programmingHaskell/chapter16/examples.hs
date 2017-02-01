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
