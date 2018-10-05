{-# LANGUAGE InstanceSigs #-}

import           Data.Char

----------------------------
-- Chapter 22 - Exercises --
----------------------------

main :: IO ()
main = undefined

-- Short Exercise : Warming Up

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = rev . cap

fmapped :: String -> String
fmapped = rev <$> cap

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

monadicTupled :: String -> (String, String)
monadicTupled = do
    revd <- rev
    capd <- cap
    return (capd, revd)

monadicTupled' :: String -> (String, String)
monadicTupled' =
    rev >>=
    \revd -> cap >>=
        \capd -> return (revd, capd)


-- Exercise : Ask
newtype Reader r a =
    Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id


-- Exercise - Reading Comprehension

-- Question 1 - liftA2
myLiftA2 :: Applicative f => (a -> b -> c)
                          -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb


-- Question 2 - asks
asks :: (r -> a) -> Reader r a
asks f = Reader $ \r -> f r


-- Question 3 - Applicative for Reader
instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) =
        Reader $ \r -> rab r $ ra r



-- Exercise - Reader Monad

instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb =
       Reader $ \r -> (runReader . aRb . ra $ r) r