import Data.Monoid
import Test.QuickCheck
import Control.Monad

-------------------------------
-- Exercise: Optional Monoid --
-------------------------------
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada x = x
    mappend x Nada = x
    mappend (Only x) (Only y) = Only (x `mappend` y)


------------------------------------
-- Exercise: Maybe Another Monoid --
------------------------------------

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = frequency [(1, return (First' Nada)),
                           (3, liftM First' (liftM Only arbitrary))]

instance Monoid (First' a) where
    mempty = First' Nada
    mappend (First' Nada) x = x
    mappend x (First' Nada) = x
    mappend x y = x

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FirstId = First' String -> Bool

testFirst :: IO ()
testFirst = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FirstId)
    quickCheck (monoidRightIdentity :: FirstId)