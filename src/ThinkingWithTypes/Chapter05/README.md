# Chapter 5 - Constraints and GADTs

## Page Contents
- [Chapter 5 - Constraints and GADTs](#chapter-5---constraints-and-gadts)
  - [Page Contents](#page-contents)
  - [See Also](#see-also)
  - [Introduction](#introduction)
  - [GADTs](#gadts)
  - [Heterogeneous Lists](#heterogeneous-lists)
    - [Using a GADT to implement the list](#using-a-gadt-to-implement-the-list)
    - [Deriving common instances](#deriving-common-instances)
    - [An alternative way of implementing instances](#an-alternative-way-of-implementing-instances)


## See Also

- [Examples (Part 1) - Source Code](Examples1.hs)
- [Examples (Part 2) - Source Code](Examples2.hs)
- [Exercises (Part 1) - Source Code](Exercises1.hs)
- [Exercises (Part 1) - Tests](../../../test/ThinkingWithTypes/Chapter05/Exercises1Spec.hs)
- [Exercises (Part 2) - Source Code](Exercises2.hs)
- [Exercises (Part 2) - Tests](../../../test/ThinkingWithTypes/Chapter05/Exercises2Spec.hs)

## Introduction

- The `Constraint` kind is reserved for things that go on the left of the fat
context arrow (`=>`):

    ```haskell
    > :k Show Int
    Show Int :: Constraint

    > :k Show
    Show :: * -> Constraint

    ```

- _Type equalities_ are enabled by the [`-XGADTs`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-GADTs) extension:

    ```haskell
    {-# LANGUAGE GADTs #-}

    five :: Int
    five = 5

    five_ :: (a ~ Int) => a
    five_
    ```

- Here, `five` and `five_` are identical as far as Haskell is concerned.  The
latter definition says that `five_` is of type `a`, along with a constraint that
`a` equals `Int`.

- Type equalities form an _equivalence relation_, i.e. they satisfy the
following laws:
  - _reflexivity_ - `a ~ a` for all types `a`.
  - _symmetry_ - `a ~ b` iff `b ~ a`.
  - _ transitivity_ - `a ~ b` and `b ~ c` implies `a ~ c`.


## GADTs

- _Generalized Algebraic Datatypes_ (_GADTs)_ are an extension that allow
explicit type signatures to be written for data constructors.

- The canonical example is a type safe syntax tree:

    ```haskell
    {-# LANGUAGE GADTs #-}

    -- The `where` turns on GADT syntax for the rest of the declaration
    data Expr a where
        -- Each line corresponds to a data constructor of `Expr`
        LitInt  :: Int -> Expr Int
        -- Each constructor takes arguments and results in an `Expr a`
        LitBool :: Bool -> Expr Bool
        Add     :: Expr Int -> Expr Int -> Expr Int
        -- Note how the concrete type returned can differ
        Not     :: Expr Bool -> Expr Bool
        -- Also note that each constructor can take a different number of args
        If      :: Expr Bool -> Expr a -> Expr a -> Expr a
    ```

- Look at how we can use this in GHCi:

    ```haskell
    > :t LitInt
    LitInt :: Int -> Expr Int

    > :t If
    If :: Expr Bool -> Expr a -> Expr a -> Expr a

    > :t LitInt 1
    LitInt 1 :: Expr Int

    > :t If (LitBool False) (LitInt 2) (LitInt 4)
    If (LitBool False) (LitInt 2) (LitInt 4) :: Expr Int
    ```

- Because GADTs allow us to specify a data constructor's type, they can be used
to constrain a type variable in certain cases.  This can be used to write a
typesafe evaluator over `Expr`:

    ```haskell
    evalExpr :: Expr a -> a

    -- This returns an `Int`...
    evalExpr (LitInt i) = i

    -- But this returns a `Bool`...
    evalExpr (LitBool b) = b

    evalExpr (Add x y) = evalExpr x + evalExpr y
    evalExpr (Not x) = not $ evalExpr x
    evalExpr (If b x y) =
      if evalExpr b
        then evalExpr x
        else evalExpr y
    ```

- In fact, GADTs are just syntactic sugar over type equalities.  The definition
of `Expr` above can equivalently be written as:

    ```haskell
    data Expr_ a
      = (a ~ Int)    => LitInt_ Int
      | (a ~ Bool)   => LitBool_ Bool
      | (a ~ Int)    => Add_ (Expr_ Int) (Expr_ Int)
      | (a ~ Bool)   => Not_ (Expr_ Bool)
      | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)
    ```

- Each data constructor of `Expr_` carries a type equality constraint, which
Haskell will require to be proven when the data constructor is called.  When
a function such as `evalExpr` pattern-matches on the data constructor with a
type equality constraint, then that constraint comes back into scope.

- So, a function of type `Expr a -> a` can return an `Int` when pattern
matching on `LitInt`, but a `Bool` when matching on `LitBool`.


## Heterogeneous Lists

### Using a GADT to implement the list

- Here's an example of using GADTs to define heterogeneous lists, which can
contain values of different types.  We can use these like this:

    ```haskell
    > :t HNil
    HNil :: HList '[]

    > :t True :# HNil
    True :# HNil :: HList '[Bool]

    > let hlist = Just "hello" :# True :# HNil
    > :t hlist
    hlist :: HList '[Maybe [Char], Bool]

    > hLength hList
    2
    ```

- Firstly, `HNil` is analogous to the regular list constructor `[]`, and
`(:#)` is analogous to the cons operator `(:)`:

    ```haskell
    {-# LANGUAGE DataKinds #-}
    {-# LANGUAGE KindSignatures #-}
    {-# LANGUAGE TypeOperators #-}

    import Data.Kind (Type)

    -- Here, `ts` has an explicit kind signature.  Adding this is a good idea if
    -- it's anything other than `*`.  This needs `DataKinds` and `KindSignatures`.
    data HList (ts :: [Type]) where
      HNil :: HList '[]
      -- This needs `TypeOperators` in order to use the `':` type cons operator
      -- Symbolically-named data constructors must start with a `:`
      (:#) :: t -> HList ts -> HList (t ': ts)

    infixr 5 :#
    ```

- We can now pattern match over this to implement various functions:

    ```haskell
    hLength :: HList ts -> Int
    hLength HNil      = 0
    hLength (_ :# ts) = 1 + hLength ts

    hHead :: HList (t ': ts) -> t
    hHead (t :# _) = t

    showBool :: HList '[_1, Bool, _2] -> String
    showBool (_ :# b :# _ :# HNil) = show b
    ```


### Deriving common instances

- GHC's stock instance deriving machinery doesn't work well with GADTs, so we
need to write our own instances for `Eq` etc:

    ```haskell
    {-# LANGUAGE FlexibleInstances #-}
    {-# LANGUAGE FlexibleContexts #-}

    instance Eq (HList '[]) where
      HNil == HNil = True

    instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
      (a :# as) == (b :# bs) = a == b && as == bs
    ```

- These instances need the [`FlexibleInstances`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances)
extension to enable arbitrarily nested types in the first line of the instance
definition (the 'instance head').

- The second case (the inductive case) also needs the [`FlexibleContexts`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleContexts) extension
to use more complex constraints in the context.


### An alternative way of implementing instances

- We had to write two instances for `Eq` above, one for the base case and one
for the inductive case.  This was partly because we needed to assert that each
type in the list had an `Eq` instance.

- As an alternative, we can use a _closed type family_ to create the constraint
programatically, by folding over all the `ts`:

    ```haskell
    {-# LANGUAGE TypeFamilies #-}
    -- ... and various other extensions

    import Data.Kind (Type, Constraint)

    type family AllEq (ts :: [Type]) :: Constraint where
      -- If empty, just return the 'unit' (empty) `Constraint`
      AllEq '[]       = ()

      -- If not, construct a `Constraint` tuple
      AllEq (t ': ts) = (Eq t, AllEq ts)
    ```

- Testing this out in GHCi:

    ```haskell
    > :set -XDataKinds
    > :kind! AllEq '[Int, Bool]

    AllEq '[Int, Bool] :: Constraint
    = (Eq Int, (Eq Bool, () :: Constraint))
    ```

- This can be generalised to any constraint:

    ```haskell
    {-# LANGUAGE TypeFamilies #-}
    {-# LANGUAGE UndecidableInstances #-}

    import Data.Kind (Type, Constraint)

    -- This is just a generalisation of `AllEq` above
    type family All (c :: Type -> Constraint)
                    (ts :: [Type]) :: Constraint where
      All c '[]       = ()
      All c (t ': ts) = (c t, All c ts)

    -- This now uses `All` to define the `Eq` instance for `HList`
    -- This needs `UndecidableInstances to support the nested
    -- constraint `All Eq ts`:
    instance All Eq ts => Eq (HList ts) where
      HNil      == HNil      = True
      (a :# as) == (b :# bs) = a == b && as == bs
    ```




-