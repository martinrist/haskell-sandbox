# Chapter 30 - When things go wrong

## 30.2 - The `Exception` class and methods

- Types that encode exceptions must have an instance of the `Exception` typeclass (from `Control.Exception`):

    ```haskell
    class (Typeable e, Show e) =>
          Exception e where
      toException :: e -> SomeException
      fromException :: SomeException -> Maybe e
      displayException :: e -> String
    ```

- `Typeable` is a typeclass that defines methods of identifying types at runtime.

- Lots of types have instances of `Exception`.  These types are sometimes simply a datatype with a single inhabitant.  Alternatively they may be a sum type of 'sub exceptions'

    ```haskell
    data BlockedIndefinitelyOnMVar = BlockedIndefinitelyOnMVar

    data ArithException =
        Overflow
      | Underflow
      | LossOfPrecision
      | DivideByZero
      | Denormal
      | RatioZeroDenominator
    ```

- `SomeException` acts as a sort of 'parent' type for all the other exception types, so that we can handle many exception types at once, without having to match all of them:

    ```haskell
    data SomeException where
        SomeException :: Exception e => e -> SomeException
    ```

- This is actually a _Generalized Abstract Datatype (GADT)_.  It can also be rewritten using _existential qualification_ as:

    ```haskell
    data SomeException =
        forall e . Exception e => SomeException e
    ```

- Note that the type variable `e` is a parameter of the _data constructor_, but doesn't appear in the _type constructor_:
    - Normally, the `forall` quantifies variables universally, but here it's applied on the _data constructor_.
    - This changes its meaning from _for all e_ to _there exists some e_ - i.e. from _universal quantification_ to _existential quantification_.
    - It means that any types that implements the `Exception` class can be that `e` and be subsumed under the `SomeException` type.

- We need existential quantification for exceptions so that we can throw various exception types without being forced to centralise and unify thm under a sum type.

- At runtime, when an exception is thrown, it starts rolling back through the stack looking for a `catch` function to handle it:
    - When it finds a `catch`, it checks to see what type of exception this `catch` handles.
    - It calls `fromException` and `cast` (from `Typeable`) to check if the type of the exception that got thrown matches the type of an exception being handled by the `catch`.
    - A `catch` that handles `SomeException` will match any exception.



## 30.3 - Catching and handling exceptions

- Most of the time when you need to worry about exceptions, you'll be in an `IO` action.

- As an example, consider this function which writes to a file 'aaa' in the current directory:

    ```haskell
    main :: IO ()
    main = do
        writeFile "aaa" "hi"
        putStrLn "wrote to file"
    ```

- If we run the file multiple times, it just overwrites the file.  But if we remove write permissions, it will throw an exception:

    ```haskell
    > fileExample
    *** Exception: aaa: openFile: permission denied (Permission denied)
    ```

- We can handle this exception by writing a handler function and using `catch`:

    ```haskell
    handler :: SomeException -> IO ()
    handler (SomeException e) = do
        print (typeOf e)
        putStrLn ("We errored! It was: " ++ show e)

    main :: IO ()
    main =
        writeFile "zzz" "hi"
            `catch` handler

    > main
    IOException
    We errored! It was: zzz: openFile: permission denied (Permission denied)
    ```

- `catch` has type `Exception e = IO a -> (e -> IO a) -> IO a`:
    - If no exception is thrown, nothing extra happens and the `IO a` at the end is the same as the `IO a` at the start.
    - If an exception _does_ get thrown, the handler function runs and has an opportunity to recover and satisfy the original `IO a` type.

- As an example of performing a different function in the handler:

    ```haskell
    handler' :: SomeException -> IO ()
    handler (SomeException e) = do
        putStrLn ("Running main caused an error! It was: "
                        ++ show e)
        -- recover by writing a different file
        writeFile "bbb" "hi"
    ```



## 30.4 - Want either? Try!

- If we want to lift exceptions out into explicit `Either` values, we can use `try`, but can't escape the fact that there's I/O in the process:

    ```haskell
    try :: Exception e => IO a -> IO (Either e a)

    willIFail :: Integer -> IO (Either ArithException ())
    willIFail denom =
        -- Using `print` because we can only handle exceptions
        -- in IO
        try $ print $ div 5 denom

    > willIFail 1
    5
    Right ()

    > willIFail 0
    Left divide by zero
    ```


## 30.5 - The unbearable imprecision of trying

- To throw an exception, use `throwIO :: Exception e => e -> IO a`

- Using `try` we can only catch certain exceptions, determined by the type of the destination `Either`:

    ```haskell
    canICatch :: Exception e
              => e
              -> IO (Either ArithException ())
    canICatch e = try $ throwIO e

    -- I can catch `DivideByZero` because it's
    -- an `ArithException`
    > canICatch DivideByZero
    Left divide by zero

    -- I can also catch `Underflow` for the same reason
    > canICatch Underflow
    Left arithmetic underflow

    -- I can't catch `StackOverflow` because it
    -- isn't an `ArithException`
    > canICatch StackOverflow
    *** Exception: stack overflow
    ```


## 30.7 - Making our own exception types

- Sometimes we want to make our own exception types, which enable us to be more precise about what's going on.

- We can declare the types and trivially give them an `Exception` instance.

- We can add context by making the types have a type parameter:

    ```haskell
    data NotDivThree =
        NotDivThree Int
        deriving (Eq, Show)

    instance Exception NotDivThree

    data NotEven =
        NotEven Int
        deriving (Eq, Show)

    instance Exception NotEven

    evenAndThreeDiv :: Int -> IO Int
    evenAndThreeDiv i
        | rem i 3 /= 0 = throwIO (NotDivThree i)
        | odd i        = throwIO (NotEven i)
        | otherwise    = return i
    ```

- If we want to handle both exceptions, we can use `catches`:

    ```haskell
    catches :: IO a -> [Handler a] -> IO a

    catchBoth :: IO Int -> IO Int
    catchBoth ioInt =
        catches ioInt
        [ Handler (\(NotEven _)    -> return maxBound)
        , Handler (\(NotDivThree_) -> return minBound)
        ]
    ```

- Alternatively, we can just create a sum type that represents both.
