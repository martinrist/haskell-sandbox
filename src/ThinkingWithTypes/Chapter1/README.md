# Chapter 1 - The Algebra Behind Types

## Page Contents
- [Chapter 1 - The Algebra Behind Types](#chapter-1---the-algebra-behind-types)
  - [Page Contents](#page-contents)
  - [See Also](#see-also)
  - [Isomorphisms & Cardinalities](#isomorphisms--cardinalities)
  - [Sum, Product & Exponential Types](#sum-product--exponential-types)
  - [Example: Tic-Tac-Toe](#example-tic-tac-toe)
  - [The Curry-Howard Isomorphism](#the-curry-howard-isomorphism)
  - [Canonical Representations](#canonical-representations)


## See Also

- [Examples - Source Code](Examples.hs)
- [Exercises - Text](Exercises.md)
- [Exercsies - Source Code](Exercises.hs)


## Isomorphisms & Cardinalities

- Each finite type can be associated with a _cardinality_, essentially the
number of inhabitants that the type has, e.g.:

    ```haskell
    data Void                -- Zero inhabiants
    data () = ()             -- One inhabitant
    data Bool = True | False -- Two inhabitants
    ```

- These statements about cardinality can be written more formally as:

    ```haskell
    |Void| = 0
    |()|   = 1
    |Bool| = 2
    ```

- Any two finite types that have the same cardinality will always be
_isomorphic_ to each other.  For two isomorphic types `s ≅ t`, there will be
two functions `to` and `from`:

    ```haskell
    to   :: s -> t
    from :: t -> s
    ```

    such that:

    ```haskell
    to . from = id
    from . to = id
    ```

- In general, for any two types with cardinality `n`, there are `n!` unique
isomorphisms between them, which correspond to the various ways we can match
up the inhabitants of the two types.


## Sum, Product & Exponential Types

- _Sum types_ correspond to addition of cardinalities.  The canonical example
of a sum type is `Either a b`:

    ```haskell
    |Either a b| = |a| + |b|
    |Maybe a| = 1 + |a|          -- Maybe = Nothing | Just a
    ```

- _Product types_ are the dual of sum types and correspond to multiplication
of cardinalities.  The canonical example is the pair `(a, b)`:

    ```
    |(a, b)| = |a| x |b|
    ```

- We can use cardinalities to express mathematical truths in terms of types.
For example to show that `a x 1 = a`, we show an isomorphism between
`(a, ())` and `(a)`:

    ```haskell
    prodUnitTo :: a -> (a, ())
    prodUnitTo a = (a, ())

    prodUnitFrom :: (a, ())
    prodUnitFrom (a, ()) = a
    ```

- Likewise, we can show `a + 0 = a` as an isomorphism between `Either a Void`
and `a`:

    ```haskell
    sumUnitTo :: Either a Void -> a
    sumUnitTo (Left  a) = a
    sumUnitTo (Right v) = absurd v

    sumUnitFrom :: a -> Either a Void
    sumUnitFrom = Left
    ```

- Function types correspond to exponentiation:

    ```
    |a -> b| = |b| ^ |a|
    ```

- Other examples of parallels between mathematical and type-based operations:
  - Subtraction - types with particular values removed
  - Division - types with some values defined equally

- Differentiation - corresponds to the type's 'one-hole contexts' - see Conor
McBride's paper ["The Derivative of a Regular Types is its Type of One-Hole Contexts"](http://strictlypositive.org/diff.pdf)

## Example: Tic-Tac-Toe

- We can use type algebra to simplify the representation of some of our types.
For example, consider a naively-implemented Tic-Tac-Toe board:

    ```haskell
    data TicTacToe a = TicTacToe
        { topLeft :: a
        , topCentre :: a
        , topRight :: a
        , midLeft :: a
        , midCentre :: a
        , midRight :: a
        , bottomLeft :: a
        , bottomCentre :: a
        , bottomRight :: a
        }

    emptyBoard :: TicTacToe (Maybe Bool)
    emptyBoard =
        TicTacToe
            Nothing Nothing Nothing
            Nothing Nothing Nothing
            Nothing Nothing Nothing
    ```

- This is a little unwieldly, but some cardinality analysis shows that:

    ```haskell
    |TicTacToe a| = |a| x |a| ... 9 times ... x |a|
                  = |a| ^ 9
                  = |a| ^ (3 * 3)
    ```

    so, `TicTacToe` is isomorphic to `(Three, Three) -> a`, or
    `Three -> Three -> a`, where `Three` is any type with 3 inhabitants:

    ```haskell
    data Three = One | Two | Three

    data TicTacToe2 a = TicTacToe2
        { board :: Three -> Three -> a
        }

    emptyBoard2 :: TicTacToe2 (Maybe Bool)
    emptyBoard2 =
        TicTacToe2 $ const $ const $ Nothing
    ```


## The Curry-Howard Isomorphism

- We can summarise our previous discussions about algebraic relationships
between types and cardinalities in the following table:

    |Algebra|Logic   |Types        |
    |-------|--------|-------------|
    |`a + b`|`a ∨ b` |`Either a b` |
    |`a x b`|`a ∧ b` |`(a, b)`     |
    |`b ^ a`|`a ⟹ b`|`a -> b`     |
    |`a = b`|`a ⟺ b`|_isomorphism_|
    |`0`    |`⊥`     |`Void`       |
    |`1`    |`⊤`     |`()`         |

- This table represents a more general isomorphism between algebra, logic and
types - the _Curry-Howard isomorphism_.


## Canonical Representations

**TODO: Complete content**