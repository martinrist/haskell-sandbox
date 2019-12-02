# Chapter 4 - Working with Types

## Page Contents
- [Chapter 4 - Working with Types](#chapter-4---working-with-types)
  - [Page Contents](#page-contents)
  - [See Also](#see-also)
  - [Type Scoping](#type-scoping)
  - [Type Applications](#type-applications)
  - [Ambiguous Types](#ambiguous-types)


## See Also

- [Examples - Source Code](Examples.hs)


## Type Scoping

- The following example, which superficially appears correct, fails to compile
because of the type annotation on `apply`:

    ```haskell
    broken :: (a -> b) -> a -> b
    broken f a = apply
      where
        apply :: b
        apply = f a
    ```

- This is because the type `b` in `apply` isn't the same `b` as that in the
signature for `broken`, so it's being interpreted as the following by the
compiler:

    ```haskell
    broken :: (a -> b) -> a -> b
    broken f a = apply
      where
        -- The compiler is expecting `apply` to be of type `c`, not `b`:
        apply :: c
        apply = f a
    ```

- In other words, type variables aren't 'scoped', until we enable the
[`ScopedTypeVariables`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ScopedTypeVariables) extension.

- When we do this, we can use the `forall a b.` quantifier to introduce a
_type scope_.  Inside this, all references to the type variables `a` and `b`
mean the same `a` and `b` as in the quantifier:

    ```haskell
    {-# LANGUAGE ScopedTypeVariables #-}

    working :: forall a b. (a -> b) -> a -> b
    working f a = apply
      where
        apply :: b
        apply = f a
    ```

- If we want to implement a function that provides a `String` corresponding to
a type's name, this is hard to do.  Some older libraries use a
[`Data.Proxy.Proxy`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Proxy.html#t:Proxy)
parameter with a phantom type:

    ```haskell
    data Proxy a = Proxy
    ```

- Then the function [`Data.Typeable.typeRef`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Typeable.html#v:typeRep) provides us with a concrete
representation of that type:

    ```haskell
    > import Data.Typeable
    > :t typeRep
    typeRep
      :: forall k (a :: k) (proxy :: k -> *).
         Typeable a => proxy a -> TypeRep

    > typeRep (Proxy :: Proxy Bool)
    Bool

    > typeRep (Proxy :: Proxy (Either Int String))
    Either Int [Char]
    ```


## Type Applications

- The [`TypeApplications`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications) extension allows us to
directly apply types to expressions:

    ```haskell
    > :set -XTypeApplications
    > :t fmap
    fmap :: Functor f => (a -> b) -> f a -> f b

    > :t fmap @Maybe
    fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b
    ```

- The `@Maybe` is part of the expression and specialises the general `fmap` to
'`fmap` over `Maybe`'.

- Types are applied in the same order that they appear in a type signature
(including its constraints and `forall` quantifiers).  So:
  - Applying `Int` to `a -> b -> a` gives `Int -> b -> Int`.
  - Applying `Int` to `forall b a. a -> b -> a` gives `a -> Int -> a`.

- You can avoid applying a type by using `@_`, so that you can apply types after
the first in order:

    ```haskell
    > :set -XTypeApplications
    > :t fmap
    -- Note that `f` is the first thing that will be applied
    fmap :: Functor f => (a -> b) -> f a -> f b

    > :t fmap @Maybe
    -- This applies `Maybe` to `f`
    fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b

    > :t fmap @_ @Int
    -- The `@_` skips the application of `f`, and applies `Int` to `a`,
    -- leaving the `f` (now `w`) polymorphic:
    fmap @_ @Int :: Functor w => (Int -> b) -> w Int -> w b
    ```

- With `TypeApplications`, types become part of a public signature, since
changing the order of type variables can break downstream code.

- Typically the hardest types to infer must come first in a signature.


## Ambiguous Types

- Using `TypeApplications` we can try to write a function that gives us the
name of a type as a `String`:

    ```haskell
    typeName :: forall a. Typeable a => String
    typeName = show . typeRep $ Proxy @a
    ```

- In the second line `Proxy @a` is shorthand for `Proxy :: Proxy a`.

- The `forall a.` quantifier is required, otherwise the reference to the type
  variable `a` isn't in scope when we try using it in `Proxy @a`.

- In the main type signature, the type `a` doesn't appear on the right-hand
  side of the context arrow (`=>`).  Type inference only works to the right of
  the context, so the type `a` can't be correctly inferred.  This is referred to
  as an _ambiguous type_.

- Code with ambiguous types will fail to compile unless we enable
  [`AllowAmbiguousTypes`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-AllowAmbiguousTypes)

- We can then call this code by using `TypeApplications` to apply the function
  to a type.  For example, with `typeName`:

    ```haskell
    > :set -XTypeApplications
    > typeName @Bool
    "Bool"

    > typeName @(Maybe [Int])
    "Maybe [Int]"
    ```

- Here's a more subtle example of an ambiguous type:

    ```haskell
    {-# LANGUAGE TypeFamilies #-}

    type family AlwaysUnit a where
        AlwaysUnit a = ()


    > :kind! AlwaysUnit String
    AlwaysUnit String :: *
    = ()
    ```

- With this definition, functions with signatures like the following are not
  ambiguous:
    - `AlwaysUnit a -> a`
    - `b -> AlwaysUnit a -> b`

  However, the function with signature `Show a => AlwaysUnit a -> String`, is
  ambiguous, since it's not clear which `Show a` instance we're asking for.
  We're not able to access the `a`, since `AlwaysUnit a` is always just `()`.

- More specifically, the issue is that `AlwaysUnit` doesn't have an inverse -
  i.e. there's no `Inverse` type family such that `Inverse (AlwaysUnit a) == a`.
  In other words, we can't learn what `a` is, given `AlwaysUnit a`.

- In the absence of such an inveerse for `AlwaysUnit a`, we have to find some
  other way of determining the otherwise ambiguous type:
  - Either by adding a `Proxy a` parameter whose only purpose is to drive
    inference.
  - Or by enabling `AllowAmbiguousTypes` at the definition site, then using
    `TypeApplications` at the call site to fill in the ambiguous parameter
    manually.