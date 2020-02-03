# Chapter 6 - Rank-N Types

## Exercise 6.3-i

_What is the rank of `Int -> forall a. a -> a`?  Hint: try adding the explicit
parentheses._

    ```haskell
    foo :: Int -> forall a. a -> a

    -- with explicit parentheses
    foo :: Int -> forall a. (a -> a)
    ```

The innermost `forall` is to the left of one `->`, so this function is _rank 1_.


## Exercise 6.3-ii

_What is the rank of `(a -> b) -> (forall c. c -> a) -> b`?  Hint: recall that
the function arrow is right-associative, so `a -> b -> c` is actually parsed as
`a -> (b -> c)`._

    ```haskell
    foo :: (a -> b) -> (forall c. c -> a) -> b`
    foo :: forall a, b. ((a -> b) -> ((forall c. (c -> a)) -> b))
    ```

The innermost `forall` is to the left of two `->`s, so this function is _rank 2_.


## Exercise 6.3-iii

_What is the rank of `((forall x. m x -> b (z m x)) -> b (z m a)) -> m a`?
Believe it or not, this is a real type signature we had to write back in the
bad old days before `MonadUnliftIO`!_

    ```haskell
    foo :: ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
    foo :: forall z . (((forall x. m x -> b (z m x)) -> b (z m a)) -> m a)
    foo :: forall z . (((forall x. (m x -> b (z m x)))) -> (b (z m a) -> m a))
    ```

The innermost `forall` is to the left of three `->`s, so this function is _rank 3_.