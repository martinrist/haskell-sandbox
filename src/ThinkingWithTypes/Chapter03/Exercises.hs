{-# LANGUAGE InstanceSigs #-}

module ThinkingWithTypes.Chapter03.Exercises where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Text.Show.Functions

-- Exercise 3-i - Functor instances

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
    fmap :: (a -> b) -> T1 a -> T1 b
    fmap f (T1 a) = T1 $ fmap f a

-- On the right-hand side here, `arbitrary :: Gen (Int -> a)`.  This is fine:
-- `Int -> a` is `Arbitrary` because (from `Test.QuickCheck.Arbitrary`):
--
--      `(CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)`
--
-- - `Int` is `CoArbitrary` (instance defined in `Test.QuickCheck.Arbitrary`)
-- - `a` is `Arbitrary` from the constraint.
instance Arbitrary a => Arbitrary (T1 a) where
    arbitrary = T1 <$> arbitrary

{-
-- Here's an example of a broken instance, which messes with the input `Int`
-- before applying it.  This instance can be used to test our `Arbitrary`
-- instance below.  The broken `Arbitrary` instance doesn't flag up any errors
-- with this broken implementation.
instance Functor T1 where
    fmap :: (a -> b) ->  T1 a -> T1 b
    fmap f (T1 ia) = T1 $
        \i -> f $ ia (i + 1)

-- This instance of `Arbitrary` is broken because it's not 'arbitrary' enough.
-- The presence of `const` means that the input to the function wrapped by `T1`
-- doesn't affect the output.
instance Arbitrary a => Arbitrary (T1 a) where
    arbitrary = T1 . const <$> arbitrary
-}

instance Show a => Show (T1 a) where
    show (T1 a) = show (a 1)

instance EqProp a => EqProp (T1 a) where
    (T1 a) =-= (T1 b) = a =-= b



newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
    fmap :: (a -> b) -> T5 a -> T5 b
    fmap f (T5 aii) = T5 $ \bi -> aii $ bi . f

-- On the right-hand side here, `arbitrary :: Gen ((a -> Int) -> Int)`.
-- `(a -> Int) -> Int` is `Arbitrary` because (from `Test.QuickCheck.Arbitrary`)
--
--      `(CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)`
--
-- - `Int` is `Arbitrary` (instance defined in `Test.QuickCheck.Arbitrary`)
-- - `(a -> Int)` is `CoArbitrary`, because:
--
--      `(Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b)`
--
--     - `a` is `Arbitrary` (from the typeclass constraint)
--     - `Int` is `CoArbitrary` (from `Test.QuickCheck.Arbitrary`)
instance Arbitrary a => Arbitrary (T5 a) where
    arbitrary = T5 <$> arbitrary


{-
-- As above, here's a broken `Functor` instance.  Since we know more about
-- the return type, we can just return a fixed `Int` value, rather than
-- needing to mess about with the input to the wrapped function.
instance Functor T5 where
    fmap :: (a -> b) -> T5 a -> T5 b
    fmap f (T5 aii) = T5 $ const 666

-- As above, here's a broken `Arbitrary` instance that isn't sufficiently
-- arbitrary to catch the errors in the broken `Functor` instance given above.
instance Arbitrary a => Arbitrary (T5 a) where
        arbitrary = T5 . const <$> arbitrary
 -}


instance Show (T5 a) where
    show (T5 aii) = show aii

instance CoArbitrary a => EqProp (T5 a) where
    (T5 a) =-= (T5 b) = a =-= b
