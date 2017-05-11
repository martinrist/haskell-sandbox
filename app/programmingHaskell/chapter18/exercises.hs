import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

andOne :: Int -> [Int]
andOne x = [x, 1]

-- Writing `bind` in terms of `fmap` and `join`

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x



-- Short Exercise : Either Monad

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (Second b) = Second (f b)
    fmap _ (First a) = First a

instance Applicative (Sum a) where
    pure = Second
    (First f) <*> _ = First f
    _          <*> (First v)  = First v
    (Second f) <*> (Second v) = Second (f v)

instance Monad (Sum a) where
    return = pure
    (First a)  >>= _ = First a
    (Second b) >>= f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (First a) =-= (First a') = a `eq` a'
    (Second b) =-= (Second b') = b `eq` b'
    _ =-= _ = property False

testEitherMonad :: IO ()
testEitherMonad = quickBatch $ monad (undefined :: Sum Int (String, Int, Int))