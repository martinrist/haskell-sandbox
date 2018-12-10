{-# LANGUAGE DeriveGeneric #-}

import Data.List (elemIndex)
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import GHC.Generics

-----------------------
-- Exercises         --
-----------------------

-- Exercise: Constant Instance
------------------------------
newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant e) where
    fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
    pure x = Constant mempty
    (Constant x) <*> (Constant x') = Constant $ x `mappend` x'


-- Exercise: List Applicative
data List a =
      Nil
    | Cons a (List a)
    deriving (Eq, Show, Generic)

instance Functor List where
    fmap _ Nil        = Nil
    fmap f (Cons x y) = Cons (f x) (fmap f y)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
    pure a = Cons a Nil
    fs <*> vs = flatMap (\f -> fmap f vs) fs


-- Exercise : ZipList Applicative

take' :: Int -> List a -> List a
take' n Nil = Nil
take' 0 _   = Nil
take' n (Cons a as) = Cons a (take' (n-1) as)

newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take' 3000 l
              ys' = let (ZipList' l) = ys
                    in take' 3000 l

instance Eq a => EqProp (List a) where
    Nil =-= ys = ys `eq` Nil
    xs =-= Nil = xs `eq` Nil
    Cons x xs =-= Cons y ys = x `eq` y .&. xs `eq` ys



instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure a = ZipList' (toList (repeat a))
    ZipList' fs <*> ZipList' vs = ZipList' $ zipListApply fs vs
        where
            zipListApply Nil _                   = Nil
            zipListApply _ Nil                   = Nil
            zipListApply (Cons f fs) (Cons v vs) = Cons (f v) (zipListApply fs vs)

toList :: [a] -> List a
toList = foldr Cons Nil

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = toList <$> arbitrary

instance (Arbitrary a, CoArbitrary a) => CoArbitrary (List a)


testZipListApplicative :: IO ()
testZipListApplicative = quickBatch (applicative (undefined :: ZipList' (List String, List String, List Int)))

testListApplicative :: IO ()
testListApplicative = quickBatch (applicative (undefined :: List (String, String, Int)))


-- Exercise - Varations on Either

Validation e a =
      Failure e
    | Success a
    deriving (Eq, Show)

data Errors =
    DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure x) = Failure x
    fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
    pure a = Success a
    (Failure f) <*> (Failure v) = Failure (f `mappend` v)
    _           <*> (Failure v) = Failure v
    (Failure f) <*> _           = Failure f
    (Success f) <*> (Success v) = Success (f v)
