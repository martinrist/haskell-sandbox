# Chapter 22 - Reader

## 22.1 - Motivation

- Often need to pass around information that's needed intermittently or universally through an application.

- Don't want to pass this around everywhere as arguments, becauase it will pollute the types of almost all functions.

- To get aruond this, we use the `Reader` monad.



## 22.2 - The `Functor` of function application

- Consider how we might traditionally compose two functions:

    ```haskell
    boop = (*2)
    doop = (+10)

    bip :: Integer -> Integer
    bip = boop . doop

    > bip 10
    40
    ```

- As an alternative to composition, we can use `fmap`:

    ```haskell
    bloop :: Integer -> Integer
    bloop = fmap boop doop
    ```

- The 'functorial context' for `fmap` here is the partially-applied function.

- Using `fmap` here lifts the one partially-applied function (`boop`) over the next (`doop`), setting up something like:

    ```haskell
    fmap boop doop x == (*2) ((+10) x)
    ```

- Now consider what happens when we extend to using `Applicative`:

    ```haskell
    bbop :: Integer -> Integer
    bbop = (+) <$> boop <*> doop

    -- or, equivalently
    duwop :: Integer -> Integer
    duwop = liftA2 (+) boop doop
    ```

- We've added another function, `(+)` to lift over teh contexts of our partially-applied functions `boop` and `doop`.

- This time, the argument gets passed to both functions 'in parallel', then the results are passed to the `(+)` function:

    ```hasekll
    > bbop 3
    19

    -- This is:
    --   ((+) <$> (*2) <*> (+10)) 3
    -- =  (+) ((*2) 3) ((+10) 3)
    -- =  (+) 6        13
    -- =  19
    ```

- Looking in more detail at `(+) <$> (*2) <*> (+10)`, we first look at the `fmap` part:

    ```haskell
    (*2)         :: Num a => a -> a
    (+)          :: Num a => a -> a -> a
    (+) <$> (*2) :: Num a => a -> a -> a

    -- The last is identical to function composition (see above):
    (+) . (*2)   :: Num a => a -> a -> a

    -- Consider what (+) <$> (*2) is, applied to two arbitrary arguments:
    > ((+) <$> (*2)) 5 3

    -- What's actually happening here?
      ((+) <$> (*2)) 5 3
    = ((+)  .  (*2)) 5 3          -- By definition of <$> for functions
    = (\x -> (+) (2 * x)) 5 3     -- since f . g = \x -> f (g x)
    = (\5 -> (+) (2 * 5)) 3       -- applying x
    = (      (+) 10     ) 3
    = 13
    ```

- So, we know what `(+) <$> (*2)` does, and that its type is `Num a => a -> a -> a`.

- Next we consider the `<*>` part, and try to concretise the fully-polymorphic type of `<*>`, knowing that the 'Applicative' context `f` is `((->) a)`:

    ```haskell
    (<*>) :: Applicative f =>        f (a -> b) ->        f a ->        f b
                              ((->) a) (a -> b) -> ((->) a) a -> ((->) a) b
                                  (a -> a -> b) ->   (a -> a) ->   (a -> b)
    ```

- We can check that this is correct in GHCi - if it wasn't, we'd get a type error:

    ```haskell
    > :t (<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)
    :t (<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)
        :: (a -> a -> b) -> (a -> a) -> (a -> b)
    ```

- So, plugging the types together from the `fmap` example and the `<*>`:

    ```haskell
    (+) <$> (*2)           :: Num a => a -> a -> a
                 <*>       ::         (a -> a -> b) ->  (a -> a) -> (a -> b)
                     (+10) :: Num a =>                  (a -> a)
    (+) <$> (*2) <*> (+10) :: Num b =>                               b -> b
    ```

- The final example is just doing the same thing, but in monadic context:

    ```haskell
    boopDoop :: Integer -> Integer
    boopDoop = do
        a <- boop
        b <- doop
        return (a + b)
    ```

- All of the above show that we can have a `Functor`, `Applicative` and `Monad` instance for partially-applied functions:
    - These are all awaiting application to one argument that will allow both functions to be evaluated.

- This is the fundamental idea of _Reader_:
    - It allows functions to be composed when all those functions are awaiting one input from a shared environment.
    - It's just another way of abstracting out function application.
    - It gives us a way of doing computation in terms of an argument that hasn't been supplied yet.
    - We use this most often when we have a constant value that will be obtained from somewhere outside our program.
    - This value will be an argument to a whole bunch of functions.
    - Using _Reader_ allows us to avoid passing that argument around explicitly.

