## Chapter 11 - Algebraic Datatypes

## 11.2 - Data declarations review

- A typical _data declaration_ consists of:
    - A _type constructor_, with or without an argument.
    - One or more _data constructors_, with or without arguments.
    - A type can be thought of as _an enumeration of constructors that have zero or more arguments_.

    ```haskell
    data [] a = [] | a : [a]
         [1]    [2]    [3]

        [1] - type constructor with argument
        [2] - data constructor for empty list
        [3] - data constructor with two arguments, an a and a [a]
    ```


## 11.3 - Data and type constructors

- _Type constructors_ are used only at _type_ level:
    - In type signatures.
    - In typeclass declarations and instances.
    - They are static and resolved at compile time.

- _Data constructors_ construct values at _term_ level:
    - These are values you can interact with at runtime.

- We can distinguish between:
    - _Constants_ - constructors without arguments (_nullary_) - called _type constants_ or _constants values_
    - _Constructors_ - other constructors with arguments.

- Examples:

    ```haskell
    -- Trivial is a type constant
    -- Trivial' is a constant value
    data Trivial = Trivial'

    -- UnaryTypeCon is a type constructor of one argument
    -- UnaryValueCon is a data constructor of one argument
    data UnaryTypeCon a = UnaryValueCon a
    ```

- Unary type constructors are awaiting a type constant to be applied to in order to resolve to a concrete type.

- Similarly, unary data constructors are awaiting a value to be applied to.


## 11.4 - Type constructors and kinds

- _Kinds_ are types of types:
    - Represented by `*`.
    - When something is a fully-applied concrete type, its kind is `*`.
    - When something is awaiting application of a single type, its kind is `* -> *`

    ```haskell
    > :k Trivial
    Trivial :: *

    > :k UnaryTypeCon
    UnaryTypeCon :: * -> *

    > :k UnaryTypeCon Int
    UnaryTypeCon Int :: *
    ```


## 11.5 - Data constructors and values

- Examples:

    ```haskell
    -- Type constant with a constant value
    data PugType = PugData

    -- Unary type constructor with a phantom type variable
    data HuskyType a = HuskyData

    -- Unary type constructor with unary data constructor
    data DogueDeBordeaux doge = DogueDeBordeax doge
    ```

