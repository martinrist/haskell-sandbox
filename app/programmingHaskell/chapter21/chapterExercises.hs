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

-- Triggers for QuickCheck
type TI = []

trigger :: TI (Int, Int, [Int])
trigger = undefined


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


testIdentityTraversable :: IO ()
testIdentityTraversable = quickBatch $ traversable trigger



-- Constant

newtype Constant a b =
    Constant { getConstant :: a}

instance Functor (Constant a) where
    fmap f = Constant . getConstant

instance Foldable (Constant a) where
    foldMap f x = mempty

instance Traversable (Constant a) where
    traverse f x = Constant <$> pure (getConstant x)

testConstantTraversable :: IO ()
testConstantTraversable = quickBatch $ traversable trigger
