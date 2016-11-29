# Chapter 9 - Lists

## 9.2 - The `List` datatype

- The `List` datatype is defined as:

    ```haskell
    data [] a = [] | a : [a]
    ```

- `[]` is the type constructor for lists as well as the data constructor.
    - The `[]` data constructor is a _nullary constructor_ because it takes no arguments.
    - The second data constructor uses the _cons_ infix operator `:`.
    - So, the `[]` data type is a _sum_ type, but the second data constructor is a _product_ type.


## 9.3 - Pattern matching on lists

- Pattern matching can be carried out on lists just like other data constructors:

    ```haskell
    > let myHead (x : _) = x
    > myHead [1, 2, 3]
    1
    
    > let myTail (_ : xs) = xs
    > myTail [1, 2, 3]
    [2, 3]
    ```

## 9.4 - `List`'s syntactic sugar

- We can use list literal syntax instead of manually constructing cons cells:

    ```haskell
    > 1 : 2 : 3 : []
    [1, 2, 3]
    > ['a', 'b', 'c']
    ['a', 'b', 'c']
    ```

## 9.5 - Using ranges to construct lists

- Lists can also be constructed using _ranges_ (inclusive), e.g.:

    ```haskell
    > [1..10]
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    
    > [1,3..10]
    [1, 3, 5, 7, 9]
    ```

- The range syntax works with any datatype that has an instance of `Enum`:

    ```haskell
    > ['t'..'z']
    "tuvwxyz"
