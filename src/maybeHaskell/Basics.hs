module Basics where

-- The simplest function definition.
-- `five` has type Int, and is equivalent to the literal 5.
five :: Int
five = 5


-- We can also annotate types using :: <type>
-- Note that type inference forces 9 and almostThird to be interpreted as Float
almostThird = (3 :: Float) / 9

exactlyThird = (3 :: Rational) / 9


-- Functions that take arguments have type declarations that look like this:
add :: Int -> Int -> Int
add x y = x + y


-- Function application is just denoted by a space - `f x` is the application of
-- f to x.


-- Higher-order functions are functions that take (or return) another function
-- `twice` takes a function f :: Int -> Int and applies it twice
-- e.g. twice (add 2) 3   => 7
twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)


-- In the above, `add 2` is a partially applied funciton.  Supplying just
-- the first argument to `add` gives a new function that adds that argument
-- to its argument:
add2 :: Int -> Int
add2 = add 2


-- As an alternative, we can put the operator `+` into a 'section', by
-- surrounding it and a single argument with parens:
add2' :: Int -> Int
add2' = (+ 2)

divideBy10 :: Double -> Double
divideBy10 = (/ 10)

divideInto10 :: Double -> Double
divideInto10 = (10 /)


-- Functions can be anonymous lambda expressions
-- Obviously redundant in the first:
square :: Int -> Int
square = \x -> x * x


power4 :: Int -> Int
power4 = twice (\x -> x * x)



-- We can define a custom data type by specifying a 'type constructor', then one
-- or more 'data constructors' (functions which produce values of the specfied
-- type).  Often, when there's just a single data constructor, it's called the
-- same as the type constructor.

-- e.g. A person consists of a name :: String and age :: Int
data Person = Person String Int

fred :: Person
fred = Person "Fred" 42

ethel :: Person
ethel = Person "Ethel" 44


-- To get the individual parts of a person out, we use pattern matching:
getName :: Person -> String
getName (Person name _) = name

getAge :: Person -> Int
getAge (Person _ age) = age


-- 'Sum types' are types that can have more than one data constructor

data Person' = PersonWithAge String Int | PersonWithoutAge String

barney :: Person'
barney = PersonWithAge "barney" 1000000

wilma :: Person'
wilma = PersonWithoutAge "wilma"


-- Again, we can use pattern matching to extract things
-- We have to provide matches for both data constructors, otherwise we'll
-- get a 'Non-exhaustive patterns' exception if we try to `getName' wilma`.
getName' :: Person' -> String
getName' (PersonWithAge name _) = name
getName' (PersonWithoutAge name) = name


-- But what do we do about getAge?
-- This is now a 'partial function', as it is not defined for all possible
-- arguments of the specified type.
getAge' :: Person' -> Int
getAge' (PersonWithAge _ age) = age


-- More generally, we might want to have a 'person with a thing' and a 'person
-- without a thing' abstraction, where 'age' is just one type of 'thing'.
-- We can parameterise the type by adding a type variable (lower-case) to the
-- type constructor.
data ParameterisedPerson a = PersonWithThing String a | PersonWithoutThing String


-- We can still create people with and without ages, but we need to specify that
-- `age` is an Int by 'fixing' the type variable 'a'.
laurel :: ParameterisedPerson Int
laurel = PersonWithThing "Stan Laurel" 125

hardy :: ParameterisedPerson Int
hardy = PersonWithoutThing "Oliver Hardy"


-- But we can also use other types of things, e.g. Strings for emails
martin :: ParameterisedPerson String
martin = PersonWithThing "Martin" "martin.rist@flybe.com"

rich :: ParameterisedPerson String
rich = PersonWithoutThing "Rich"



-- Haskell's `Maybe` type is similar to our Person example.
-- (commented out as it's defined elsewhere)
-- data Maybe a = Nothing | Just a

definitelyAnInteger :: Maybe Int
definitelyAnInteger = Just 5

definitelyNotAnInteger :: Maybe Int
definitelyNotAnInteger = Nothing


-- We can use pattern matching to extract the value and handle `Nothing`.
whatsInside :: Maybe Int -> String
whatsInside (Just a) = "The argument contains " ++ show a
whatsInside (Nothing) = "The argument is empty"


-- Example usaga, take a predicate (a function from type a to `Bool`) and a
-- list of a's, and return Just the first match (or Nothing if nothing)
find :: (a -> Bool) -> [a] -> Maybe a
find _ []                    = Nothing
find predicate (first:rest)  = if predicate first
                                 then Just first
                                 else find predicate rest


-- Here, returning a `Maybe a` clearly distinguishes between the case where
-- an a satisfying the predicate has been found or not, and forces the caller
-- to handle it **by a compile-time type check**.

