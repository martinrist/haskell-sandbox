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
