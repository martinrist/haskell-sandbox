# Chapter 7 - More functional patterns

## 7.2 - Arguments and parameters

- Functions may appear to have multiple parameters, but because of currying, all functions take one argument and return one result.

- All Haskell values can be arguments to functions - a value that can be used as an argument to a function is called a _first-class_ value.  This includes functions.

- Applying a function binds its parameters to values - type parameters become bound to a type, and function variables are bound to a value.

- Binding also occurs in things like `let` expressions and `where` clauses:

    ```haskell
    bindExp :: Integer -> String
    bindExp x = let y = 5 in
                    "the integer was: " ++ show x
                    ++ " and y was: " ++ show y

    > bindExp 2
    "the integer was: 2 and y was: 5"

    > bindExp 3
    "the integer was: 3 and y was: 5"
    ```

- Function arguments may not be visible if they have been _shadowed_ by an inner `let`:

    ```haskell
    bindExp :: Integer -> String
    bindExp x = let x = 10; y = 5 in
                    "the integer was: " ++ show x
                    ++ " and y was: " ++ show y

    > bindExp 2
    "the integer was: 10 and y was: 5"

    > bindExp 3
    "the integer was: 10 and y was: 5"
    ```

- Haskell is _lexically-scoped_ - resolving the value for a named entity depends on the location in the code and the lexical context.


## 7.3 - Anonymous functions

- Anonymous functions are defined using a lambda-style syntax:

    ```haskell
    > (\x -> x * 3) 10
    30

    > let triple = (\x -> x * 3)
    > triple 5
    15
    ```


## 7.4 - Pattern matching

- _Pattern matching_ is a way of matching values against patterns and binding variables to successful matches:
    - Patterns can include things like undefined variables, literals, list syntax.
    - Allows you to dispatch different behaviour based on data.
    - Patterns are matched against values, not types.

- Simplest is to pattern match on numbers:

    ```haskell
    isItTwo :: Integer -> Bool
    isItTwo 2 = True
    isItTwo _ = False
    ```

    Here, `_` is the 'universal' match, which ensures that the function is not partial.

- Pattern matching proceeds in order, so the following will not work as expected:

    ```haskell
    isItTwo :: Integer -> Bool
    isItTwo _ = False
    isItTwo 2 = True
    ```

- Pattern matching can take place against _data constructors_:

    ```haskell
    newtype Username = Username String
    newtype AccountNumber = AccountNumber Integer

    data User = UnregisteredUser
              | RegisteredUser Username AccountNumber

    printUser :: User -> IO ()
    printUser UnregisteredUser = putStrLn "UnregisteredUser"
    printUser (RegisteredUser (Username name) (AccountNumber acctNum))
            = putStrLn $ name ++ " " ++ show acctNum
    ```

- Also against _sum types_:

    ```haskell
    data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
                     deriving (Eq, Show, Enum)

    nextDay :: DayOfWeek -> DayOfWeek
    nextDay Mon = Tue
    nextDay Tue = Wed
    ...
    nextDay Sun = Mon
    ```

- And _product types_, and _tuples_:

    ```haskell
    addNumbers :: Num a => (a, a) -> a
    addNumbers (x, y) = x + y

    getFirst :: (a, a) -> a
    getFirst (x, _) = x
    ```

- We can use _as-patterns_ to match on parts of an argument, but still refer to the entire original value:

    ```haskell
    f :: Show a => (a, b) -> IO (a, b)
    f t@(a, _) = do
        print a
        return t

    > f (1, 2)
    1
    (!, 2)
    ```


## 7.5 - Case expressions

- Case expressions are similar to `if-then-else`, enabling functions to return different values based on inputs:

    ```haskell
    nextDay :: DayOfWeek -> DayOfWeek
    nextDay d =
        case d == Sun of
             True  -> Mon
             False -> succ d

    palindrome :: [Char] -> [Char]
    palindrome xs =
        case xs == reverse xs of
             True  -> "yes"
             False -> "no"
    ```

- Can also be combined with `where` clauses to improve readability or reuse the value:

    ```haskell
    palindrome' xs =
        case y of
             True  -> "yes"
             False -> "no"
        where y = xs == reverse xs
    ```


## 7.6 - Higher-order functions

- _Higher-order functions_ are functions that either:
    - Take another function as a paramter, or
    - Return a function, or
    - Both of the above.

- Classic example is `flip`, which reverses the order of its arguments:

    ```haskell
    flip :: (a -> b -> c) -> b -> a -> c
    flip f x y = f y x

    -- Alternative implementation using anonymous functions
    flip' f = \x y -> f y x

    > (-) 10 1
    9
    > (flip (-)) 10 1
    -9
    ```

- Remember that `->` is right-associative, so the type signature:

    ```haskell
    flip :: (a -> b -> c) -> b -> a -> c
    ```

    is equivalent to:

    ```haskell
    flip :: (a -> b -> c) -> (b -> a -> c)
    ```

    which shows how it takes a function of type `a -> b -> c` and returns a function of type `b -> a -> c`.


## 7.7 - Guards

- _Guards_ are a syntactic pattern that use truth values to decide between possible results:

    ```haskell
    myAbs :: Integer -> Integer
    myAbs x
        | x < 0         = (-x)
        | otherwise     = x
    ```

- Each _guard case_ is separated by a pipe character followed by the condition to be evaluated:
    - The first guard case that evaluates to `True` gets executed.
    - `otherwise` is just `True`, so it functions as a default case.

- We can also use `where` clauses in guard blocks:

    ```haskell
    grade :: (Fractional a, Ord a) => a -> Char
    grade x
        | y > 0.9   = 'A'
        | y >= 0.8  = 'B'
        | y >= 0.7  = 'C'
        | y >= 0.59 = 'D'
        | y < 0.59  = 'F'
        where y = x / 100
    ```


## 7.8 - Function composition

- Function composition is a type of higher-order function that allows us to combine functions together.

    ```haskell
    > :t (.)
    (.) :: (b -> c) -> (a -> b) -> a -> c
    
    > let sumThenNegate = negate . sum
    > sumThenNegate [1, 2, 3, 4, 5]
    -15
    ```

- Often, the `$` operator is used to avoid the need for extra parentheses:

    ```haskell
    > negate . sum $ [1, 2, 3, 4, 5]
    -15
    > (negate . sum) [1, 2, 3, 4, 5]
    -15
    ```

- This is even more important when composing lots of functions:

    ```haskell
    > take 5 . filter odd . enumFrom $ 3
    [3, 5, 7, 9, 11]
    ```


## 7.9 - Pointfree style

- _Pointfree style_ refers to a style of composing functions without specifying arguments:

    ```haskell
    > let f = negate . sum
    > f [1, 2, 3, 4, 5]
    -15
    ```

    ```haskell
    f :: Int -> [Int] -> Int
    f z xs = foldr (+) z xs

    -- Alternative, pointfree implementation:
    f = foldr (+)
    ```
