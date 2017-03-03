# Chapter 17 - Applicative

## 17.1 - Introduction

- `Applicative` is a _monoidal functor_ - like a combination of `Monoid` and `Functor`:
    - `Monoid` gives us a means of mashing two values of the same type together.
    - `Functor` gives us a means of applying a function _over_ some structure (e.g. list).

- `Applicative` combines these two:
    - It allows us to have function application 'lifted' over structure (like `Functor`)
    - However, with `Applicative` the function being applied is also embedded in some structure.

- Because both the function _and_ the value its applied to are both embedded in some structure, we have to combine those structures, which is where the `Monoid` part comes in.


## 17.2 - Defining `Applicative`

- The typeclass declaration for `Applicative` is:

    ```haskell
    class Functor f => Applicative f where
        pure  :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
    ```

- Note that the `f` that represents the structure is constrained by the `Functor` typeclass:
    - i.e. for the `f` to have an instance of `Applicative` it must also have a `Functor` typeclass.

- `pure` just embeds something into the applicative structure:

    ```haskell
    > pure 1 :: [Int]
    [1]

    > pure 1 :: Maybe Int
    Just 1
    ```

- `<*>`, also called _apply_ takes functions embedded in structure and applies those to values embedded in structure:

    ```haskell
    > :t (<*>)
    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    ```


## 17.3 - `Functor` vs `Applicative`

- Comparing the types of `<*>` and `fmap` (or `<$>`), we can see the difference:

    ```haskell
    (<$>) :: Functor f     =>   (a -> b) -> f a -> f b
    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    ```

- 


*TODO:* Add in something about `liftA`, `liftA2` and `liftA3`

