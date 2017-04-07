module PhhhbbtttEither where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Prelude hiding (Left, Right)


-- Question 2 - PhhhbbtttEither b a

data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
        a <- arbitrary
        return $ Left a

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq


instance Functor (PhhhbbtttEither b) where
    fmap f (Left a) = Left $ f a
    fmap _ (Right b) = Right b

instance Applicative (PhhhbbtttEither b) where
    pure = Left
    (Right b) <*> _         = Right b
    _         <*> (Right b) = Right b
    (Left f)  <*> (Left a)  = Left $ f a

instance Monad (PhhhbbtttEither b) where
    return = pure
    (Left a) >>= f = f a
    (Right b) >>= _ = Right b

main :: IO ()
main = do
    let trigger = undefined :: PhhhbbtttEither String (Int, String, Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger