# Chapter 5 - Constraints and GADTs

## Page Contents
- [Chapter 5 - Constraints and GADTs](#chapter-5---constraints-and-gadts)
  - [Page Contents](#page-contents)
  - [See Also](#see-also)
  - [Introduction](#introduction)
  - [GADTs](#gadts)
  - [Heterogeneous Lists](#heterogeneous-lists)


## See Also

- [Examples - Source Code](Examples.hs)
- [Exercises - Text](Exercises.md)
- [Exercises - Source Code](Exercises.hs)
- [Exercises - Tests](../../../test/ThinkingWithTypes/Chapter05/ExercisesSpec.hs)


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

