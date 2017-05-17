import           GHC.Generics
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
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

identityTrigger :: Identity (Int, Int, [Int])
identityTrigger = undefined

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

testIdentityTraversable :: IO ()
testIdentityTraversable = quickBatch $ traversable identityTrigger



-- Constant

newtype Constant a b =
    Constant { getConstant :: a}
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f = Constant . getConstant

instance Foldable (Constant a) where
    foldMap f x = mempty

instance Traversable (Constant a) where
    traverse f x = Constant <$> pure (getConstant x)

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = do
        a <- arbitrary
        return $ Constant a

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq

constantTrigger :: Constant (Int, Int, [Int]) (Int, Int, [Int])
constantTrigger = undefined

testConstantTraversable :: IO ()
testConstantTraversable = quickBatch $ traversable constantTrigger


-- Maybe

data Optional a =
      Nada
    | Yep a
    deriving (Eq, Ord, Show)

instance Functor Optional where
    fmap _ Nada    = Nada
    fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
    foldMap _ Nada    = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada    = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary =
        frequency [(1, return Nada),
                   (3, fmap Yep arbitrary)]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

optionalTrigger :: Optional (Int, Int, [Int])
optionalTrigger = undefined

testOptionalTraversable :: IO ()
testOptionalTraversable = quickBatch $ traversable optionalTrigger
