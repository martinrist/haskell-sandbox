# Chapter 18 - Monad

## 18.1 - Monad

- A _monad_ is an _applicative functor_ with an extra feature that makes them different from and more powerful than either `<*>` or `fmap` alone.


## 18.2 - `Monad` typeclass and operations

- The `Monad` typeclass is defined as:

    ```haskell
    class Applicative m => Monad m where
        (>>=)  :: m a -> (a -> m b) -> m b
        (>>)   :: m a -> m b -> m b
        return :: a -> m a
    ```

- The `Applicative m` typeclass constraint means that you can derive `Functor` and `Applicative` in terms of `Monad`, just as you can derive `Functor` in terms of `Applicative`.

    ```haskell
    fmap f xs = xs >>= return . f

    > fmap (+1) [1..3]
    [2, 3, 4]

    > [1..3] >>= return . (+1)
    [2, 3, 4]
    ```

- Easiest operation in `Monad` is `return :: a -> m a`, which is Monad's version of `pure`.

- `(>>) :: m a -> m b -> m b` sequences two actions while discarding the resulting value of the first (i.e. the `m a`).

- `(>>=) :: m a -> (a -> m b) -> m b` is called 'bind', and it contains the things that are special about `Monad`.

- Compare the various signatures of `<$>`, `<*>` and `>>=`, and `=<<` (a.k.a. `flip (>>=)`) (easier to see the similarity):

    ```haskell
    fmap       :: Functor f     =>   (a -> b) -> f a -> f b
    <*>        :: Applicative f => f (a -> b) -> f a -> f b
    =<<        :: Monad f       =>   a -> f b -> f a -> f b

    >>=        :: Monad f       => f a        -> (a -> f b)
    ```

- Unlike `fmap`, which takes an `a -> b` as the 'mapping' function, bind takes an `a -> f b`.  Consider what happens if we use `fmap` with a mapping function that 'adds' extra structure:

    ```haskell
    andOne :: Int -> [Int]
    andOne x = [x, 1]

    > andOne 10
    [10, 1]

    > fmap andOne [4, 5, 6]
    [[4, 1], [5, 1], [6, 1]]

    > :t fmap andOne [4, 5, 6]
    fmap andOne [4, 5, 6] :: [[Int]]
    ```

- See how we have an extra layer of monadic structure, which has been generated by our mapping function:
    - In this case, a list of lists.
    - If we just want a single layer (`[Int]`) we need to remove the extra layer of structure.

- For lists, we can remove using `concat`:

    ```haskell
    > :t concat
    concat :: Foldable t => t [a] -> [a]

    > concat $ fmap andOne [4, 5, 6]
    [4, 1, 5, 1, 6, 1]
    ```

- `Monad` is like a generalisation of `concat`, with the unique part being the `join` function from `Control.Monad`:

    ```haskell
    > import Control.Monad
    > :t join
    join :: Monad m => m (m a) -> m a
    ```

- So, bind is a combination of the mapping function (which adds structure), and `join`, which takes it back away again:

    ```haskell
    -- This is actually flip (>>=)
    bind :: Monad m => (a -> m b) -> m a -> m b
    bind f x = join $ fmap f x
    ```

## 18.3 - `do` Syntax and Monads

- We've seen `do` syntax earlier, when sequencing operations within the `IO` monad:

    ```haskell
    sequencing :: IO ()
    sequencing = do
        putStrLn "blah"
        putStrLn "another thing"
    ```

- Consider the following 'sequencing' functions which are used in desugaring `do`:

    ```haskell
    (*>) :: Applicative f => f a -> f b -> f b
    (>>) :: Monad m       => m a -> m b -> m b
    ```

- The intial `do` syntax can be desugared using both `>>` and `*>`, both of which give the same result:

    ```haskell
    sequencing' :: IO ()
    sequencing' =
        putStrLn "blah" >>
        putStrLn "another thing"

    sequencing'' :: IO ()
    sequencing'' =
        putStrLn "blah" *>
        putStrLn "another thing"
    ```

- The alternative form of `do` syntax uses variable binding, which can be desugared using `>>=`:

    ```haskell
    binding :: IO ()
    binding = do
        name <- getLine
        putStrLn name

    binding' :: IO ()
    binding = getLine >>= putStrLn
    ```

- Consider how the types fit together in the above - see how the flipped arguments in `>>=` help the sequencing:

    ```haskell
    (>>=)    :: Monad m => m  a      -> (a      -> m  b) -> m  b
                           |  |          |         |  |     |  |
    getLine  ::            IO String     |         |  |     |  |
    putStrLn ::                          String -> IO ()    |  |
    binding  ::                                             IO ()
    ```

- Consider why we can't just `fmap` `putStrLn` over `geLine` in the above:

    ```haskell
    > putStrLn <$> getLine
    foo
    -- No output above...

    > :t (putStrLn <$> getLine)
    (putStrLn <$> getLine) :: IO (IO ())
    ```

- Since `putStrLn` has type `String -> IO ()`, using `fmap` has put an extra level of `IO` in the result, which is why it doesn't work.  We can make this work using `join`, or `>>=`:

    ```haskell
    import Control.Monad (join)
    > join $ putStrLn <$> getLine
    foo
    foo

    -- Note the argument flipping again
    > getLine >>= putStrLn
    foo
    foo
    ```

- Here, `join` has merged the effects of `getLine` and `putStrLn` into a single `IO` action.

- Another example of `do` desugaring is when we both sequence and bind:

    ```haskell
    bindingAndSequencing :: IO ()
    bindingAndSequencing = do
        putStrLn "Please enter your name:"
        name <- getLine
        putStrLn ("Hello " ++ name)

    -- Desugared form
    bindingAndSequencing' :: IO ()
    bindingAndSequencing' =
        putStrLn "Please enter your name:" >>
        getLine >>=
        \name -> putStrLn ("Hello " ++ name)
    ```

- As we nest more and more, the value of `do` becomes more apparent:

    ```haskell
    twoBinds :: IO (
    twoBinds = do
        putStrLn "Please enter your name:"
        name <- getLine
        putStrLn "Please enter your age:"
        age <- getLine
        putStrLn ("Hello " ++ name ++ "! You are " ++ age ++ " years old")

    -- Desugared form
    twoBinds' :: IO ()
    twoBinds' =
        putStrLn "Please enter your name:" >>
        getLine >>=
        \name -> putStrLn "Please enter your age:" >>
        getLine >>=
        \age ->
        putStrLn ("Hello " ++ name ++ "! You are " ++ age ++ " years old")
    ```


## 18.4 - Examples of Monad use

### List

- The signatures of `>>=` and `return` specialised to the List monad are:

    ```haskell
    (>>=) :: Monad m => m  a -> (a -> m  b) -> m  b
    (>>=) ::            [] a -> (a -> [] b) -> [] b

    -- or, more commonly:

    (>>=) ::            [a]  -> (a -> [b])  -> [b]

    return :: Monad m => a -> m  a
    return ::            a -> [] a
    return ::            a -> [a]
    ```

- An example of using `do` notation with Lists:

    ```haskell
    twiceWhenEven :: [Integer] -> [Integer]
    twiceWhenEven xs = do
        -- This binds individual values out of the list input
        x <- xs

        -- This is the (a -> m b), or in this case Integer -> [Integer]
        if even x
            then [x*x, x*x]
            else [x*x]

    > twiceWhenEven [1..3]
    [1, 4, 4, 9]
    ```

### Maybe

- The signatures of `>>=` and `return` specialised to the 'Maybe' monad are:

    ```haskell
    (>>=) :: Monad m =>     m a -> (a ->     m b) ->     m b
    (>>=) ::            Maybe a -> (a -> Maybe b) -> Maybe b

    return :: Monad m => a ->     m a
    return ::            a -> Maybe a
    ```

- Say we have a `Cow` datatype, and some basic validation functions:

    ```haskell
    data Cow = Cow {
         name   :: String
       , age    :: Int
       , weight :: Int
      } deriving (Eq, Show)

    noEmpty :: String -> Maybe String
    noEmpty "" = Nothing
    noEmpty s  = Just s

    noNegative :: Int -> Maybe Int
    noNegative n | n >= 0    = Just n
                 | otherwise = Nothing

    weightCheck :: Cow -> Maybe Cow
    weightCheck c =
        let w = weight c
            n = name c
        in if n == "Bess" && w > 499
              then Nothing
              else Just c
    ```

- Let's say we want a function `mkCow :: String -> Int -> Int -> Maybe Cow` that does validation and (possibly) creates a `Cow` instance.  The naive way would be to have a deeply-nested set of `case`s that check the various validation results.

- The nicer way is to use `do` syntax:

    ```haskell
    mkCow :: String -> Int -> Int -> Maybe Cow
    mkCow name age weight = do
        namey   <- noEmpty name
        agey    <- noNegative age
        weighty <- noNegative weight
        weightCheck (Cow namey agey weighty)
    ```

- We can't do this using `Applicative` because the final function (`weightCheck`) has signature `Cow -> Maybe Cow` - i.e. it depends on a pre-existing value and returns more monadic structure in its return type.

- In general, `do` syntax that looks like the following can be rewritten using `Applicative`:

    ```haskell
    doSomething = do
        a <- f
        b <- g
        c <- h
        pure (a, b, c)
    ```

- On the other hand, something like the following needs `Monad` because `g` and `h` are producing monadic structure based on values that can only be obtained by depending on values from previous monadic structure:

    ```haskell
    doSomething' n = do
        a <- f n
        b <- g a
        c <- h b
        pure (a, b, c)
    ```


### Either

- The signatures of `>>=` and `return` specialised to the 'Either' monad are:

    ```haskell
    (>>=) :: Monad m =>        m a -> (a ->        m b) ->        m b
    (>>=) ::            Either e a -> (a -> Either e b) -> Either e b

    return :: Monad m => a ->        m a
    return ::            a -> Either e a
    ```

- With `Either`, we can do validation, and fail with an error (in the `Either`'s `Left` data constructor):

    ```haskell
    -- Some types for a software shop
    type Founded = Int
    type Coders = Int

    data SoftwareShop =
        Shop {
            founded     :: Founded
          , programmers :: Coders
        } deriving (Eq, Show)

    -- validation functions with possible error conditions
    data FoundedError =
          NegativeYears Founded
        | TooManyYears Founded
        | NegativeCoders Coders
        | TooManyCoders Coders
        | TooManyCodersForYears Founded Coders
        deriving (Eq, Show)

    validateFounded :: Int -> Either FoundedError Founded
    validateFounded n
        | n < 0     = Left $ NegativeYears n
        | n > 500   = Left $ TooManyYears n
        | otherwise = Right n

    validateCoders :: Int -> Either FoundedError Coders
    validateCoders n
        | n < 0     = Left $ NegativeCoders n
        | n > 5000  = Left $ TooManyCoders n
        | otherwise = Right n

    mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
    mkSoftware years coders = do
        founded     <- validateFounded years
        programmers <- validateCoders coders
        -- This contains validation based on the output of previous steps
        if programmers > div founded 10
           then Left $ TooManyCodersForYears founded programmers
           else Right $ Shop founded programmers
    ```

- Note that `Either` always short-circuits on the _first_ failure - it must do this because later values can depend on previous ones:

    ```haskell
    > mkSoftware 200 10
    Right (Shop {founded = 200, programmer = 10})

    > mkSoftware (-1) 2
    Left (NegativeYears (-1))

    -- Note that this doesn't also give us `NegativeCoders`
    > mkSoftware (-1) (-2)
    Left (NegativeYears (-1))
    ```


## 18.5 - Monad laws

- Identity laws state that `return` should be neutral and not perform any computations:

    ```haskell
    -- right identity
    m >>= return      = m

    -- left identity
    return x >>= f    = f x
    ```

- Associativity is similar to the associativity laws for the other typeclasses, but looks different due to the nature of `>>=`:

    ```hasell
    (m >>= f) >> = g   =   m >>= (\x -> f x >>= g)
    ```

- In the above, the left-hand side looks as expected, but the right-hand side is a little stranger:
    - `\x -> f x >>= g)` is just a function that has type `a -> m b`.
    - It takes `x`, then applies `f :: a -> m b` to it through normal function application, yielding an `m b`.
    - It then uses `>>=` to apply `g` to the resulting `b`.

- As before, we can test the Monad laws using the `checkers` library:

    ```haskell
    > quickBatch (monad [(1, 2, 3)])

    monad laws:
      left  identity: +++ OK, passed 500 tests.
      right identity: +++ OK, passed 500 tests.
      associativity:  +++ OK, passed 500 tests.
    ```

## 18.6 - Application and composition

- With `Functor` and `Applicative`, the 'mapping' function doesn't add structure (it's `a -> b`, not `a -> m b`), so composition 'just works':

    ```haskell
    > fmap ((+1) . (+2)) [1..5]
    [4, 5, 6, 7, 8]

    > fmap (+1) . fmap (+2) $ [1..5]
    [4, 5, 6, 7, 8]
    ```

- Consider the types we'd want for composing monadic functions, compared to normal composition:

    ```haskell
    (.)   ::            (b ->   c) -> (a ->   b) -> (a ->   c)
    mcomp :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
    ```

- This clearly won't work, because the types don't align:

    ```haskell
    mcomp f g a = f (g a)
    ```

- However, we can `fmap` `f` over `g a` to give an extra level of structure, which we then remove using `join`:

    ```haskell
    mcomp f g a = join (f <$> (g a))
    ```

- But this is just `>>=`:

    ```haskell
    mcomp f g a = g a >>= f
    ```

- `Control.Monad` contains the _Kleisli composition_ function `>=>`, which is designed to do just this (albeit with flipped arguments to make it easier to see the sequencing):

    ```haskell
    (.)      ::            (b ->   c) -> (a ->   b) -> a ->   c
    flip (.) ::            (a ->   b) -> (b ->   c) -> a ->   c
    (>=>)    :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
    ```

- An example using the `IO` Monad:

    ```haskell
    -- Outputs a prompt, then gets input
    sayHi :: String -> IO String
    sayHi prompt = do
        putStrLn prompt
        getLine

    -- Takes a readable String and returns it wrapped in IO
    readM :: Read a => String -> IO a
    readM = return.read

    -- Prompts for, then reads age
    -- Uses Kleisli composition to stitch together sayHi and readM
    getAge :: String -> IO Int
    getAge = sayHi >=> readM

    askForAge :: IO Int
    askForAge = getAge "Hello! How old are you?"
    ```