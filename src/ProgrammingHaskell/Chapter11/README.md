# Chapter 11 - Algebraic Datatypes

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
    - Can define a typeclass instance for `newtype`s that differ from the instances for their underlying type.


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

- There are two things we can do with a value:
    - Generate or construct it.
    - Match on it and consume it.

- Example of constructing a trivial type with a nullary constructor, equivalent to the `()` unit type:

    ```haskell
    data GuessWhat = Chickenbutt deriving (Eq, Show)
    
    trivialValue :: GuessWhat
    trivialValue = Chickenbutt
    ```

- Example of a unary type constructor that contains one unary data constructor:

    ```haskell
    data Id a = MkId a deriving (Eq, Show)

    idInt :: Id Integer
    idInt = MkId 10
    ```

- We can also pass a function type, such as `a -> a`, as the type parameter:

    ```haskell
    idIdentity :: Id (a -> a)
    idIdentity = MkId $ \x -> x
    ```

- Next, we can construct a value for a home-made product type:

    ```haskell
    data Product a b = Product a b deriving (Eq, Show)
    type Awesome = Bool
    type Name = String

    person :: Product Name Awesome
    person = Product "Simon" True
    ```

- Alternatively, a home-made sum type:

    ```haskell
    data Sum a b = First a | Second b deriving (Eq, Show)
    data Twitter = Twitter deriving (Eq, Show)
    data AskFm = AskFm deriving (Eq, Show)

    socialNetwork :: Sum Twitter AskFm
    socialNetwork = First Twitter
    ```


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


## 11.16 - Lists are polymorphic

- The list datatype definition shows that it is fully polymorphic in the type of the list's elements:

    ```haskell
    data [] a = [] | a : [a]
    ```

- Operators with a non-alphanumeric name are infix by default:
    - All non-alpha arithmetic functions (e.g. `+`, `*`).
    - Contrast `div` and `mod` which are arithmetic functions, but are prefix by default.

- Any operator that starts with a `:` must be an infix type or data constructor:
    - All infix data constructors must start with `:`.
    - The type constructor of functions (`->`) is the only infix type constructor that doesn't start with a colon.
    - Type constructors can't be `::` as this is reserved.

- We can use other non-alphanumeric characters for infix type or data constructors, e.g.:

    ```haskell
    data Product a b =
        a :&: b
        deriving (Eq, Show)

    > :t 1 :&: 2
    1 :&: 2 :: (Num a, Num b) => Product a b
    ```

- An alternative way to define the list type using _cons cells_:

    ```haskell
    data List a = Nil | Cons a (List a)

    -- `Nil` on its own doesn't apply the type parameter
    > let nil = Nil
    > :t nil
    nil :: List a

    > let oneItem = (Cons "hello" Nil)
    > :t oneItem
    oneItem :: List [Char]

    -- `List` is a higher-kinded type
    > :k List
    List :: * -> *

    -- `List Int` has been fully-applied
    > :k List Int
    List Int :: *
    ```


## 11.17 - Binary tree

- We can define a recursive data type that represents a _binary tree_:

    ```haskell
    data BinaryTree a =
        Leaf
      | Node (BinaryTree a) a (BinaryTree a)
      deriving (Eq, Ord, Show)
    ```

- Inserting into binary trees is often done such that lesser nodes are to the left:

    ```haskell
    insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
    insert' b Leaf = Node Leaf b Leaf
    insert' b (Node left a right)
        | b == a = Node left a right
        | b < a  = Node (insert' b left) a right
        | b > a  = Node left a (insert' b right)
    ```

- To map over a binary tree:

    ```haskell
    mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
    mapTree _ Leaf = Leaf
    mapTree f (Node left a right) =
        Node (mapTree f left) (f a) (mapTree f right)
    ```

- Tree traversal can be _pre-order_, _in-order_ or _post-order_:

    ```haskell
    preorder :: BinaryTree a -> [a]
    preorder Leaf = []
    preorder (Node left a right) = a : [] ++ preorder left ++ preorder right

    inorder :: BinaryTree a -> [a]
    inorder Leaf = []
    inorder (Node left a right) = inorder left ++ a : [] ++ inorder right

    postorder :: BinaryTree a -> [a]
    postorder Leaf = []
    postorder (Node left a right) = postorder left ++ postorder right ++ a : []
    ```

- We can then implement `foldTree` thus:

    ```haskell
    foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
    foldTree f z = foldr f z . inorder
    ```
