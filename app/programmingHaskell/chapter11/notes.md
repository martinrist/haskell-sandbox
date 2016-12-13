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


## 11.6 - What's a type and what's data?

- Types are static and resolve at compile time - they are known before runtime, either through explicit declaration or type inference.

- Type constructors appear on the left-hand side of the `=` in data declarations, data constructors on the right:
    - The exception is where a data constructor has a reference to a concrete type as an argument:

    ```haskell
    data Price = Price Integer deriving (Eq, Show)
    data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
    data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

    data Vehicle = Car Manufacturer Price
                 | Plane Airline
                 deriving (Eq, Show)
    ```


## 11.7 - Data constructor arities

- Examples of some common data declarations with different 'arities':

    ```haskell
    -- nullary
    data Example0 = Example0 deriving (Eq, Show)

    -- unary
    data Example1 = Example1 Int deriving (Eq, Show)

    -- product of Int and String
    data Example2 = Example2 Int String deriving (Eq, Show)
    ```


## 11.8 - What makes these datatypes algebraic?

- Datatypes in Haskell are said to be _algebraic_ because we can describe them in terms of _sum_ and _product_ operations.

- The _cardinality_ of a datatype is the number of possible values it defines, e.g.:
    - `Bool` can have 2 values - `True` or `False` => cardinality = 2.
    - `Int8` can have values from `-128` to `127` => cardinality = 256.
    - `Integer` is unbounded => cardinality = infinite.
    - `[Bool]` can be of any length => cardinality = infinite, although the cardinality of the members is 2.

- Simple types with nullary data constructors have a cardinality of 1:

    ```haskell
    -- Only possible value is `MakeExample` => cardinality = 1
    data Example = MakeExample deriving Show
    ```

- Datatypes with unary data constructors have the same cardinality as the type used in the type argument.  In other words, unary constructors are an identity function for cardinality:

    ```haskell
    -- Cardinality of `Goats` = cardinality of `Int`
    data Goats = Goats Int deriving (Eq, Show)
    ```


## 11.9 - `newtype`

- We can use `newtype` to mark a type that can only ever have a single unary data constructor:

    ```haskell
    newtype Goats = Goats Int deriving (Eq, Show)

    newtype Cows = Cows Int deriving (Eq, Show)
    ```

- These are differnt from type declarations marked with the `data` keyword, as well as type synonyms marked using the `type` keyword.

- Cardinality of a `newtype` is the same as that of the type it contains.

- A `newtype` cannot be a product type, sum type or contain a nullary constructor.

- A `newtype` has various advantages over a basic `data` declaration:
    - No runtime overhead, since it reuses the representation of the type it contains.
    - Can define a typelcass instance for `newtype`s that differ from the instances for their underlying type.


## 11.10 - Sum types

- Sum types are represented with `|`, which represents logical disjunction / 'or'.

- The cardinality of a sum type is the sum of the cardinalities of their data constructors.


## 11.11 - Product types

- Product types are types with more than one type argument.  They correspond to logical conjunction / 'and'.

- The cardinality of a product type is the product of the cardinalities of the type parameters:

    ```haskell
    -- This sum type has cardinality 3
    data QuantumBool = QuantumTrue
                     | QuantumFalse
                     | QuantumBoth deriving (Eq, Show)

    -- This product type has cardinality 9 (i.e. 3 * 3)
    data TwoQs = MkTwoQs QuantumBool QuantumBool
                    deriving (Eq, Show)
    ```

- _Records_ in Haskell are product types with additional syntax to provide convenient field accessors:

    ```haskell
    -- Without records, you have to write accessors explicitly
    data Person = MkPerson String Int deriving (Eq, Show)

    getName :: Person -> String
    getName (MkPerson s _) = s

    getAge :: Person -> Int
    getAge (MkPerson _ i) = i

    -- With records

    data PersonRecord = PersonRecord { name :: String
                                     , age :: Int }
                                     deriving (Eq, Show)

    > :t name
    name :: PersonRecord -> String

    > :t age
    age :: PersonRecord -> String

    > let me = PersonRecord "Name" 42
    > me
    PersonRecord {name = "Name", age = 42}

    > name me
    "Name"

    > age me
    42
    ```


## 11.12 - Normal form

- A datatype definition is said to be in _normal form_ if it is expressed as a 'sum of products'.

- To express something as _normal form_, we can use the fact that products distribute over sums:

    ```haskell
    data Fiction = Fiction deriving Show
    data Nonfiction = Nonfiction deriving Show

    -- `BookType` is a sum of two types
    data BookType = FictionBook Fiction
                  | NonfictionBook Nonfiction
                  deriving Show

    type AuthorName = String

    -- `Author` is a product of `AuthorName` and `BookType`)
    data Author = Author (AuthorName, BookType)

    -- This is equivalent
    data Author' = Fiction AuthorName | Nonfiction AuthorName deriving (Eq, Show)
    ```


## 11.13 - Constructing & deconstructing values

- TO BE COMPLETED


## 11.14 - Function type is exponential

- When calculating inhabitants of types, the type of functions (`(->)`) is the exponent operator:
    - `|a -> b|` is `|b| ^ |a|` where `| |` represents the cardinality of a type.
    - This is the number of possible implementations of the function.
    
- For a function `a -> b -> c`:
    - `|a -> b -> c| = (|c| ^ |b|) ^ |a| = |c| ^ (|b| * |a|)`
    - This is equal to `|(a, b) -> c|` - i.e. treating it as a function of a tuple argument (itself a product type).


## 11.15 - Higher-kinded datatypes

- Recall from earlier that kinds are the types of type constructors:
    - A concrete type such as `Integer` has a kind of `*`.
    - Types such as `[a]` have a kind of `* -> *`, and are waiting for a single `*` (corresponding to the type argument `a`) before they are fully-applid.
    - These are known as _higher-kinded types_.

- Example for tuple types (like product type):

    ```haskell
    > :k (, , , )
    (, , , ) :: * -> * -> * -> * -> *
    
    > :k (Int, , , )
    (Int, , , ) :: * -> * -> * -> *
    
    > :k (Int, String, , )
    (Int, String, , ) :: * -> * -> *

    > :k (Int, String, Bool, )
    (Int, String, Bool, ) :: * -> *
    
    > :k (Int, String, Bool, String)
    (Int, String, Bool, String) :: *
    ```

