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
    ```


## 9.6 - Extracting portions of lists

- `take` and `drop` take or drop elements from the start of a list

    ```haskell
    take :: Int -> [a] -> [a]
    drop :: Int -> [a] -> [a]

    > take 7 ['a'..'z']
    "abcdefg"

    > take 3 []
    []

    > drop 5 [1..10]
    [6, 7, 8, 9, 10]

    > drop 4 []
    []
    ```

- `take` can operate on infinite lists:

    ```haskell
    > take 3 (enumFrom 1)
    [1, 2, 3]
    ```

- `splitAt` cuts a list into two parts at the element specified, and makes a tuple of two lists:

    ```haskell
    splitAt :: Int -> [a] -> ([a], [a])

    > splitAt 5 [1..10]
    ([1, 2, 3, 4, 5], [6, 7, 8, 9, 10])
    ```

- Higher-order functions `takeWhile` and `dropWhile` take a predicate and take / drop items from a list that meet some conditions.  They stop at the first element that doesn't satisfy the condition:

    ```haskell
    takeWhile :: (a -> Bool) -> [a] -> [a]
    dropWhile :: (a -> Bool) -> [a] -> [a]

    > takeWhile (<3) [1..10]
    [1, 2]

    > takeWhile (<8) (enumFromTo 5 15)
    [5, 6, 7]

    > takeWhile (>6) [1..10]
    []

    > dropWhile (<3) [1..10]
    [3, 4, 5, 6, 7, 8, 9, 10]
    ```


