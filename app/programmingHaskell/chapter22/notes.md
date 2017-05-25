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
