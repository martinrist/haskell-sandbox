# Chapter 2 - Terms, Types & Kinds - Exercises

## Exercise 2.1.3-i

_If `Show Int` has kind `Constraint`, what's the kind of `Show`?_

`Show` takes a type and returns a constraint, so its kind is `* -> Constraint`:

  ```haskell
  > :k Show
  Show :: * -> Constraint
  ```


## Exercise 2.1.3-ii

_What is the kind of `Functor`?_

The type signature of `fmap` is `Functor f => (a -> b) -> f a -> f b`.  From
this we can see the the `f` in `Functor f` is applied to a type, so `f` must
be of kind `* -> *`.

Therefore `Functor` is of kind `(* -> *) -> Constraint`:

  ```
  > :k Functor
  Functor :: (* -> *) -> Constraint
  ```


## Exercise 2.1.3-iii

_What is the kind of `Monad`?_

We can apply a similar argument to `Functor` above.  The signature of `(>>=)` is
`Monad m => m a -> (a -> m b) -> m b`, so `Monad :: * -> *`.

Thus `Monad` is of kind `(* -> *) -> Constraint`:

  ```
  > :k Monad
  Monad :: (* -> *) -> Constraint
  ```


## Exercise 2.1.3-iv

_What is the kind of `MonadTrans`?_

`MonadTrans` has a method `lift :: Monad m => m a -> t m a`, where `t` is the
`MonadTrans` instance.

So `t` can be applied to a monad `m` and a type `a`, and returns a type, so
its kind must be `(* -> *) -> * -> *`.

So the kind of `MonadTrans` is `((* -> *) -> * -> *) -> Constraint`:

  ```haskell
  > import Control.Monad.Trans.Class
  > :k MonadTrans
  MonadTrans :: ((* -> *) -> * -> *) -> Constraint
  ```


## Exercise 2.4-i

_Write a closed type family to compute `Not`_

See [Exercises.hs](Exercises.hs).  With this defined, we can do:

  ```haskell
  > :kind Not
  Not :: Bool -> Bool

  > :kind! Not 'True
  Not 'True :: Bool
  = 'False
  ```