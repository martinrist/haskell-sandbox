{-# LANGUAGE FlexibleContexts #-}

import           Data.Monoid              ((<>))
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Function

------------------------------------
-- Chapter 21 - Chapter Exercises --
------------------------------------
main :: IO ()
main = undefined

-- Traversable instances
------------------------
-- Identity
newtype Identity a =
    Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

identityTrigger :: Identity (Int, Int, [Int])
identityTrigger = undefined

instance Arbitrary a =>
         Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance Eq a =>
         EqProp (Identity a) where
    (=-=) = eq

testIdentityTraversable :: IO ()
testIdentityTraversable = quickBatch $ traversable identityTrigger

-- Constant
newtype Constant a b = Constant
    { getConstant :: a
    } deriving (Eq, Show)

instance Functor (Constant a) where
    fmap f = Constant . getConstant

instance Foldable (Constant a) where
    foldMap f x = mempty

instance Traversable (Constant a) where
    traverse f x = Constant <$> pure (getConstant x)

instance Arbitrary a =>
         Arbitrary (Constant a b) where
    arbitrary = do
        a <- arbitrary
        return $ Constant a

instance Eq a =>
         EqProp (Constant a b) where
    (=-=) = eq

constantTrigger :: Constant (Int, Int, [Int]) (Int, Int, [Int])
constantTrigger = undefined

testConstantTraversable :: IO ()
testConstantTraversable = quickBatch $ traversable constantTrigger

-- Maybe
data Optional a
    = Nada
    | Yep a
    deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada    = Nada
    fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
    foldMap _ Nada    = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada    = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance Arbitrary a =>
         Arbitrary (Optional a) where
    arbitrary = frequency [(1, return Nada), (3, fmap Yep arbitrary)]

instance Eq a =>
         EqProp (Optional a) where
    (=-=) = eq

optionalTrigger :: Optional (Int, Int, [Int])
optionalTrigger = undefined

testOptionalTraversable :: IO ()
testOptionalTraversable = quickBatch $ traversable optionalTrigger

-- List
data List a
    = Nil
    | Cons a
           (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x ys) = Cons (f x) (fmap f ys)

instance Foldable List where
    foldMap _ Nil         = mempty
    foldMap f (Cons x ys) = mappend (f x) (foldMap f ys)

instance Traversable List where
    traverse _ Nil         = pure Nil
    traverse f (Cons x ys) = Cons <$> f x <*> traverse f ys

toList :: [a] -> List a
toList = foldr Cons Nil

instance Arbitrary a =>
         Arbitrary (List a) where
    arbitrary = toList <$> arbitrary

instance Eq a =>
         EqProp (List a) where
    Nil =-= ys = ys `eq` Nil
    xs =-= Nil = xs `eq` Nil
    Cons x xs =-= Cons y ys = x `eq` y .&. xs `eq` ys

listTrigger :: List (Int, Int, [Int])
listTrigger = undefined

testListTraversable :: IO ()
testListTraversable = quickBatch $ traversable listTrigger

-- Three
data Three a b c =
    Three a
          b
          c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Eq a, Eq b, Eq c) =>
         EqProp (Three a b c) where
    (=-=) = eq

threeTrigger :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
threeTrigger = undefined

testThreeTraversable :: IO ()
testThreeTraversable = quickBatch $ traversable threeTrigger

-- Three'
data Three' a b =
    Three' a
           b
           b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
    foldMap f (Three' a b b') = mappend (f b) (f b')

instance Traversable (Three' a) where
    traverse f (Three' a b b') = Three' a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        return $ Three' a b b'

instance (Eq a, Eq b) =>
         EqProp (Three' a b) where
    (=-=) = eq

three'Trigger :: Three' (Int, Int, [Int]) (Int, Int, [Int])
three'Trigger = undefined

testThree'Traversable :: IO ()
testThree'Traversable = quickBatch $ traversable three'Trigger

-- S
data S n a =
    S (n a)
      a
    deriving (Eq, Show)

instance Functor n =>
         Functor (S n) where
    fmap f (S x y) = S (fmap f x) (f y)

instance Foldable n =>
         Foldable (S n) where
    foldMap f (S n a) = foldMap f n <> f a

instance Traversable n =>
         Traversable (S n) where
    traverse g (S n a) = S <$> traverse g n <*> g a

instance (Arbitrary (n a), CoArbitrary (n a), Arbitrary a, CoArbitrary a) =>
         Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) =>
         EqProp (S n a) where
    (=-=) = eq

sTrigger :: S Maybe (Int, Int, [Int])
sTrigger = undefined

testSTraversable :: IO ()
testSTraversable = quickBatch $ traversable sTrigger

-- Tree

data Tree a =
      Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty        = Empty
    fmap f (Leaf a)     = Leaf $ f a
    fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
    foldMap _ Empty        = mempty
    foldMap f (Leaf a)     = f a
    foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
    traverse g Empty        = pure Empty
    traverse g (Leaf a)     = Leaf <$> g a
    traverse g (Node l a r) = Node <$> traverse g l <*> g a <*> traverse g r


doubleIfOdd :: (Eq a, Integral a) => a -> Maybe a
doubleIfOdd x = if (x `mod` 2) == 0 then Nothing else Just (x * 2)

mkTree :: Arbitrary a => Gen (Tree a)
mkTree = do
    a <- arbitrary
    l <- mkTree
    r <- mkTree
    frequency [ (1, return Empty)
              , (2, return $ Leaf a)
              , (2, return $ Node l a r) ]

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Tree a) where
    arbitrary = mkTree

instance Eq a => EqProp (Tree a) where
    (=-=) = eq

treeTrigger :: Tree (Int, Int, [Int])
treeTrigger = undefined

testTreeTraversable :: IO ()
testTreeTraversable = quickBatch $ traversable treeTrigger

testAll :: IO ()
testAll = do
    putStrLn "\n\nTesting Traversable instance for Identity"
    testIdentityTraversable
    putStrLn "\n\nTesting Traversable instance for Constant"
    testConstantTraversable
    putStrLn "\n\nTesting Traversable instance for Optional"
    testOptionalTraversable
    putStrLn "\n\nTesting Traversable instance for List"
    testListTraversable
    putStrLn "\n\nTesting Traversable instance for Three"
    testThreeTraversable
    putStrLn "\n\nTesting Traversable instance for Three'"
    testThree'Traversable
    putStrLn "\n\nTesting Traversable instance for S"
    testSTraversable
    putStrLn "\n\nTesting Traversable instance for Tree"
    testTreeTraversable
