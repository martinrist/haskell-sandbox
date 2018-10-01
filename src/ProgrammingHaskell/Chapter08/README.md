# Chapter 8 - Recursion

## 8.1 - Recursion

- _Recursion_ is defining a function in terms of itself, via self-referential expressions.

- Because of the anonymity of functions, the lambda calculus does not appear on the surface to have support for recursion:
    - However, this can be achieved using the [Y Combinator or fixed-point combinator](http://mvanier.livejournal.com/2897.html).


## 8.2 - Factorial

- The factorial function `n!` has a classically recursive implementation, consisting of a base case (`0! = 1`) and a recursive case (`n! = n * (n-1)!`).  These cases are easily expressed using pattern matching:

    ```haskell
    factorial :: Integer -> Integer
    factorial 0 = 1
    factorial n = n * factorial (n - 1)
    ```


## 8.3 - Bottom

- `bottom` is a term used in Haskell to refer to computations that do not successfully result in a value, either:
    - Computations that failed with an error:

        ```haskell
        f :: Bool -> Int
        f True = error "blah"
        f False = 0

        > f False
        0
        > f True
        *** Exception: blah
        ```

    - Computations that failed to terminate.

        ```haskell
        > let x = x in x
        ...
        ```
    - _Partial functions_:

        ```haskell
        f :: Bool -> Int
        f False = 0

        > f False
        0
        > f True
        *** Exception...
                Non-exhaustive patterns in function f
        ```

- One way to make a partial function into a total function is to use the `Maybe` datatype:

    ```haskell
    data Maybe a = Nothing | Just a

    f :: Bool -> Maybe Int
    f False = Just 0
    f True = Nothing
    ```


## 8.4 - Fibonacci numbers

- Fibonacci numbers are another classic case of recursion, but with two bases cases / recursive steps:
    - Base cases - `fib(0) = 0` and `fib(1) = 1`
    - Recursive case - `fib(n) = fib(n-1) + fib(n-2)`

    ```haskell
    fib :: Integral a => a -> a
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n - 1) + fib (n - 2)
    ```


## 8.5 - Integral division from scratch

- A recursive implementation of integral division that returns a tuple of `(quotient, remainder)`:

    ```haskell
    dividedBy :: Integral a => a -> a -> (a, a)
    dividedBy num denom = go num denom 0
        where go n d count
              | n < d     = (count, n)
              | otherwise = go (n - d) d (count + 1)
    ```

- This uses a common Haskell idiom called a _go function_, where the `go` function (defined in a `where` clause) accepts more arguments than the top-level function, one of which is a counter.
