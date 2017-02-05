------------------------------
-- Exercises: Heavy Lifting --
------------------------------
import Test.QuickCheck
import Test.QuickCheck.Function

-- Exercise 1
a :: [Int]
a = fmap (+1) $ read "[1]" :: [Int]

-- Exercise 2
b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- Exercise 3
c :: Int -> Int
c = fmap (*2) (\x -> x - 2)

-- Exercise 4
d :: Int -> String
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- Exercise 5
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++) . show) ioi
        in fmap (*3) changed


----------------------------------
-- Exercises: Instances of Func --
----------------------------------

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g  . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

-- Exercise 1 - Identity
newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

type IntToInt = Fun Int Int

exercise1 :: IO ()
exercise1 = do
    quickCheck $ \x -> functorIdentity (x :: Identity Int)
    quickCheck (functorCompose' :: Identity Int -> IntToInt -> IntToInt -> Bool)


