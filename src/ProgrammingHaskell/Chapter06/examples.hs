------------------------------
--  Chapter 6 - Typeclasses --
------------------------------
module ProgrammingHaskell.Chapter06.Examples where

main :: IO ()
main = undefined

------------------------------------
-- 6.5 - Writing typeclass instances
------------------------------------
-- With no `deriving` clause, we can't even write `Trivial == Trivial'...
data Trivial =
    Trivial

-- ... so, we have to write the typeclass instance.  For `Eq` we can either
-- write an implementation for `==` or for `\=`
instance Eq Trivial where
    Trivial == Trivial = True

-- A more complex example where we define equality for days of the week
data DayOfWeek
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun
    deriving (Show)

-- Note the final line, which is there to ensure the pattern is exhaustive
-- If not, `==` would be a 'partial function'
-- We can warn about these using `:set -Wall` in GHCI
instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

data Date =
    Date DayOfWeek
         Int

instance Eq Date where
    (==) (Date weekDay monthNum) (Date weekDay' monthNum') =
        weekDay == weekDay' && monthNum == monthNum'

-- As an alternative to the above, we can just use `deriving Eq` on the type
-- definition.  This defaults to requiring each comopnent to be equal
data Date' =
    Date' DayOfWeek
          Int
    deriving (Eq)

data Identity a =
    Identity a
    deriving (Show)

-- This needs the `Eq a =>` constraint in order to be able to call `v == v'`
instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

------------
-- 6.6 - Num
------------
-- This won't work, because we can't deduce that `x` is an instance
-- of `Fractional` which is what's needed for `/`
-- divideThenAdd :: Num a => a -> a -> a
-- However by applying the type class constraint `Fractional a`
-- everything compiles
divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1

--------------
-- 6.11 - Show
--------------
-- We could declare `Mood` as an instance of `Show`
-- and provide an implementation for `show`
data Mood =
    Blah

instance Show Mood where
    show _ = "Blah"

-- Alternatively, we could just derive `Show` and
-- rely on the default implementation of `show`,
-- which just prints out the value
data Mood' =
    Meh
    deriving (Show)

-------------------------------------
-- 6.12 - Typeclass Instance Dispatch
-------------------------------------
-- A typeclass defines a set of functions and / or values
class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer

-- Types have instances of that typeclass
newtype Age =
    Age Integer
    deriving (Eq, Show)

instance Numberish Age where
    fromNumber = Age
    toNumber (Age n) = n

newtype Year =
    Year Integer
    deriving (Eq, Show)

instance Numberish Year where
    fromNumber = Year
    toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where
    integerOfA = toNumber a
    integerOfAPrime = toNumber a'
    summed = integerOfA + integerOfAPrime

-- After `sumNumberish` is applied to an `Age` type, it knows that the
-- remaining arguments are also of type `Age`
foo :: Age -> Age
foo = sumNumberish (Age 10)

-------------------------------
-- 6.13 - Gimme More Operations
-------------------------------
-- This doesn't work, because we don't know enough about a to use +
-- add :: a -> a -> a
-- add x y = x + y
-- Instead we need to say that `a` is an instance of the `Num` typeclass:
add :: Num a => a -> a -> a
add x y = x + y

-- If we do something odd, and require a method from another typeclass
-- (`Ord`), then we'll get another error
-- addWeird :: Num a => a -> a -> a
-- addWeird x y = if x > 1
--                  then x + y
--              else
--                  x
-- `Num` doesn't imply `Ord`, so we have to add another typeclass
-- constraint in a tuple:
addWeird :: (Num a, Ord a) => a -> a -> a
addWeird x y =
    if x > 1
        then x + y
        else x

-- This isn't going to typecheck, because `a` isn't an instance of `Eq`
-- check' :: a -> a -> Bool
-- check' a a' = a == a'
-- However, instead of adding `Eq` as a typeclass constraint, we can add
-- `Ord`, since `Eq` is a superclass of `Ord`
check' :: Ord a => a -> a -> Bool
check' a a' = a == a'
