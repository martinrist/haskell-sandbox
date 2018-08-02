{-# LANGUAGE InstanceSigs #-}

import           Control.Applicative (liftA2)

-----------------------------------------
-- Chapter 22 - Reader - Examples --
-----------------------------------------
main :: IO ()
main = undefined

-- 22.2 - Examples
------------------
-- `boop` and `doop` are just two simple Integer -> Integer functions
boop = (* 2)

doop = (+ 10)

-- We can compose them in the normal manner
bip :: Integer -> Integer
bip = boop . doop

-- We can also compose using `fmap`
bloop :: Integer -> Integer
bloop = fmap boop doop

-- Extend to using Applicatives
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

-- .. or equivalently
duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- The same, but in Monadic context
boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)

-- 22.5 - The Reader newtype
----------------------------
newtype Reader r a = Reader
    { runReader :: r -> a
    }

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader $ \r -> f (ra r)

-- 22.6 - Applicative instance for functions
newtype HumanName =
    HumanName String
    deriving (Eq, Show)

newtype DogName =
    DogName String
    deriving (Eq, Show)

newtype Address =
    Address String
    deriving (Eq, Show)

data Person = Person
    { humanName :: HumanName
    , dogName   :: DogName
    , address   :: Address
    } deriving (Eq, Show)

data Dog = Dog
    { dogsName    :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

bigBird :: Person
bigBird = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Sesame Street")

martin :: Person
martin = Person (HumanName "Martin")
                (DogName "Tinder")
                (Address "Exeter")

-- Taking a Person and creating their Dog, without using `Reader`:
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- And the same using Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- Alternatively, with `liftA2`
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address



-- 22.7 - The Monad of Functions

-- Increment all values in the Functor f
mapInc :: (Functor f, Num a) => f a -> f a
mapInc r = fmap (+1) r

-- Returns a tuple of arg 1, and the length of arg 2
returnWithArg2Length :: Foldable f => t -> f a -> (t, Int)
returnWithArg2Length r t = (r, length t)


-- Reduce `returnWithArg2Length` to one parameter
returnWithArgLength :: Foldable t => t a -> (t a, Int)
returnWithArgLength r = (r, length r)


-- Example with Dog and Person
getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addr <- address
    return $ Dog name addr
