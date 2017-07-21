# Chapter 29 - IO

## 29.2 - Issues with common IO explanations

- Often `IO` and `Monad` are conflated in discussions:
    - `IO` _does_ have a `Monad` instance.
    - However, `IO` does more than just being a `Monad`.

- Another option is to explain `IO` in terms of the `State` (or `ST`) monad.

- Looking at the underlying types, the parallel with `State` is tempting:

    ```haskell
    newtype State s a =
        State { runState :: s -> (a, s) }

    newtype IO a =
        IO (State# RealWorld -> (# State# RealWorld, a #))
    ```

- However, you don't usefully see or interact with the underlying `State#`:
    - It's not `State` in the sense that you use `State`, `StateT` or `ST`.

- The `State` here is a signalling mechanism to tell GHC what order the IO actions are in.

- Also, `RealWorld` is "represented by nothing at all" - it uses zero bits of memory:
    - The state tokens underlying the IO type are erased at compile time.



## 29.3 - The reason we need the `IO` type

- `IO` exists primarily to give us a way to order operations and to disable some of the sharing features.

- GHC can normally do a number of things to optimise performance or memory usage, e.g.:
    - Reordering operation
    - Delaying evaluation
    - Sharing named values
    - Duplicating code via inlining

- The main purpose of the `IO` type is to turn off most of these abilities, because they would interfere with reliable operation and ordering of I/O.

- IO actions are enclosed within nested lambdas, which is the only way to ensure correct sequencing, and to guarantee that the following outputs `"123"` as expected:

    ```haskell
    main :: IO ()
    main = do
        putStr   "1"
        putStr   "2"
        putStrLn "3"
    ```

- The reason we use a `Monad` instance for `IO`, together with the `do` notation, is to abstract away all the nested lambda noise underlying `IO`.



## 29.4 - Sharing

- As discussed above, `IO` also turns off a lot of the sharing of named references:
    - It can't turn all of it off, because all programs have a `main :: IO ()` method.

- Normally, when a function returns a value of a specified type, we're saying "If this is evaluated, we will have a value of this type as a result":
    - With `IO a`, however, we don't get back an `a`.
    - Instead we get a description of how to get an `a` from the 'real world', possibly performing side-effects along the way.

- In this situation (where we don't have a value, but a 'recipe' to get a value), then sharing the value doesn't make sense:

    ```haskell
    -- Consider the following, from Data.Time.Clock
    getCurrentTime :: IO UTCTime
    ```

- In the above, it's the `IO` that prevents the 'current' time being fixed the first time it's called.



## 29.5 - `IO` doesn't disable sharing for everything

- `IO` only disables sharing for the terminal value that it reduces to:
    - Values that are not dependent on `IO` for their evaluation can still be shared.
    - This is still the case within a larger IO action, like `main`.

- For example, using `trace` again:

    ```haskell
    blah :: IO String
    blah = return "blah"

    blah' = trace "outer trace" blah

    woot :: IO String
    woot = return (trace "inner trace") "woot")

    main :: IO ()
    main = do
        b <- blah'
        putStrLn b
        putStrLn b
        w <- woot
        putStrLn w
        putStrLn w

    -- Note that the traces only appear once,
    -- so b and w are still shared
    > main
    outer trace
    blah
    blah
    inner trace
    woot
    woot
    ```


## 29.7 - `IO`s `Functor`, `Applicative` and `Monad` instances

- `IO` has a `Functor` instance which gives it an `fmap` that takes an action and constructs an action that performs the same effects but transforms the value:

    ```haskell
    fmap :: (a -> b) -> IO a -> IO b

    > randomIO :: IO Int
    -2788551936633825385

    -- The result has been incremented before returning
    > fmap (+1) randomIO :: IO Int
    -7247933619526368632
    ```

- `IO` also has an `Applicative` instance:

    ```haskell
    (<*>) :: IO (a -> b) -> IO a -> IO b

    > (++) <$> getLine <*> getLine
    hello
    world
    "helloworld"

    -- Add two random integers
    > (+) <$> randomIO <*> randomIO :: IO Int
    6475332562802816220
    ```

- For `IO`, both `pure` and `return` can be read as an effect-free embedding of a value in a 'recipe-creating environment'.

- GHCi can run IO actions and print their results:

    ```haskell
    > let embedInIO = return :: a -> IO a
    > embedInIO 1
    1
    ```

- However, having something in two layers of `IO` doesn't do anything:

    ```haskell
    > embedInIO . embedInIO $ 1
    -- No result...

    -- We need to use `join` to turn the `IO (IO a)` into an `IO a`
    > join . embedInIO . embedInIO $ 1
    1
    ```

- The `Monad` instance for `IO` allows the effects performed by the _outer_ IO action to influence the inner action, e.g.:

    ```haskell
    huehue :: IO (Either (IO Int) (IO ()))
    huehue = do
        t <- getCurrentTime
        let (_, _, dayOfMonth) =
                    toGregorian (utctDay t)
        case even dayOfMonth of
            True ->
                return $ Left randomIO
            False ->
                return $ Right (putStrLn "no soup for you")
    ```

- The IO action returned here is dependent on having checked the current time, and determining whether the day is even or odd.

- This is not possible with `Applicative` alone.
