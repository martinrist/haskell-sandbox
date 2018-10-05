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



## 9.7 - List comprehensions

- _List comprehensions_ allow us to generate a new list from one or more existing lists (called _generators_):

    ```haskell
    > [ x ^ 2 | x <- [1..10]]
    [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
    ```

- List comprehensions can optionally take _predicates_ that filter elemnets from the generator:

    ```haskell
    > [ x ^ 2 | x <- [1..10], rem x 2 == 0]
    [4, 16, 36, 64, 100]
    ```

- List comprehensions can have multiple generators.  The last generator is exhausted first, then we go onto the next value from the first generator:

    ```haskell
    > [ x ^ y | x <- [1..5], y <- [2, 3]]
    [1, 1, 4, 8, 9, 27, 16, 64, 25, 125]
    ```

- Strings are lists, so list comprehensions work with string as well:

    ```haskell
    > [x | x <- "Three Letter Acronym", elem x ['A'..'Z']]
    "TLA"
    ```


## 9.8 - Spines and nonstrict evaluation

- Given a list [1, 2, 3], this can be represented in various ways:

    ```
    [1, 2, 3]           - the list literal

    1 : 2 : 3 : []      - a series of 'cons cells'

    1 : (2 : (3 : []))  - parenthesised version of above

      :                 - 'spine' and 'value' representation
     / \
    1   :
       / \
      2   :
         / \
        3   []
    ```

- It is possible to evaluate just the spine of the list without evaluating individual values.  It's also possible to evaluate only part of the spine of a list and not the rest of it.

- GHCi offers a command `:sprint`, which prints a value without forcing its evaluation:

    ```haskell
    > let blah = enumFromTo 'a' 'z'

    > :sprint blah
    blah = _
    ```

- We can start to force evaluation of the list by taking elements from it.  This evaluates one cons cell `:` and the first value `'a'`:

    ```haskell
    > take 1 blah
    "a"

    > :sprint blah
    blah = 'a' : _
    ```

- Taking a second vlaue forces evaluation of the second cons cell and value:

    ```haskell
    > take 2 blah
    "ab"

    > :sprint blah
    blah = 'a' : 'b' : _
    ```

- Values in Haskell can be in _normal form_ or _weak head normal form_:
    - _Normal form (NF)_ - the expression is fully evaluated.
    - _Weak head normal form (WHNF)_ - the expression is only evaluated as far as is necessary to reach a data constructor, or a lambda awaiting an argument.

- NF is a subset of WHNF - for an expression in WHNF, further evaluation may be possible once another argument is provided:
    - If no further inputs are possible, the expression is also in NF.

- Examples:

    ```haskell
    (1, 2)              - WHNF & NF - clearly fully evaluated.
    (1, 1 + 1)          - WHNF but not NF - the (+) could be evaluated but hasn't been yet.
    \x -> x * 10        - WHNF & NF - (*) cannot be reduced further until the outer x -> ... has been applied.
    "ab" ++ "cd"        - Neither WHNF nor NF - outermost component of the expression is an unapplied function.
    (1, "ab" ++ "cd")   - WHNF but not NF.
    ```

- When we define a list and all its values (e.g. through a list literal), it is in NF and all values are known:

    ```haskell
    > let num :: [Int]; num = [1, 2, 3]
    > :sprint num
    num = [1, 2, 3]
    ```

- On the other hand, constructing a list through functions (e.g. `enumFromTo`) or ranges results in a list in WHNF:

    ```haskell
    > let num :: [Int]; num = [1..10]
    > :sprint num
    num = _
    ```

- `length` is only strict in the spine, meaning it only forces evaluation of the spine, not the values.  Unfortunately, `:sprint` behaves as if it were strict in the values as well:

    ```haskell
    > length blah
    26

    > :sprint blah
    blah = "abcdefghijklmnopqrstuvwxyz"
    ```

- We can demonstrate that `length` is only spine-strict by making a value _bottom_ (with `undefined`) and noting that it doesn't blow up:

    ```haskell
    > let x = [1, undefined, 3]
    > length x
    3

    > foldr (+) 0 x
    *** Exception: Prelude.undefined
    ```

- If we define our own version of `length`:

    ```haskell
    length' :: [a] -> Integer
    length' [] = 0
    length' (_:xs) = 1 + length xs
    ```

    This only forces the `:` data constructors and the final `[]`, so the list looks like this:

    ```
            :       <-|
           / \        |
      |-> _   :     <-|
      |      / \      | These got evaluated (forced)
      |->   _   :     |
      |        / \    |
      |->     _  [] <-|
      |
      | These did not
    ```

- However, `length` will throw an error on a _bottom_ value if it's part of the spine:

    ```haskell
    > let x = [1] ++ undefined ++ [3]
    > length x
    *** Exception: Prelude.undefined
    ```


## 9.9 - Transforming lists of values

- Generally speaking, we want to use higher-order functions for transforming lists of data, rather than manually recursing over them.

- To apply a function to elements, use `map` for `[]`, or `fmap` for datatypes that have an instance of `Functor`:

    ```haskell
    > :t map
    map :: (a -> b) -> [a] -> [b]

    > map (+1) [1, 2, 3]
    [2, 3, 4]

    > :t fmap
    fmap :: Functor f => (a -> b) -> f a -> f b

    > fmap (+1) (Just 2)
    Just 3

    > fmap (+1) Nothing
    Nothing
    ```

- `map` is defined as follows in `Base`:

    ```haskell
    map :: (a ->b) -> [a] -> [b]
    map _ []     = []
    map f (x:xs) = f x : map f xs
    ```

- `map` is non-strict in values, so they're only evaluated if taken:

    ```haskell
    > map (+1) [1, 2, undefined]
    [2, 3,*** Exception: Prelude.undefined

    > take 2 $ map (+1) [1, 2, undefined]
    [2, 3]
    ```


## 9.10 - Filtering lists of values

- `filter` allows us to remove values from a list that don't meet a predicate:

    ```haskell
    filter :: (a -> Bool) -> [a] -> [a]
    filter _ []    = []
    filter pred (x:xs)
        | pred x    = x : filter pred xs
        | otherwise = filter pred xs

    > filter odd [1..10]
    [1, 3, 5, 7, 9]

    > filter (\x -> (rem x 2) == 0) [1..10]
    [2, 4, 6, 8, 10]
    ```


## 9.11 - Zipping lists

- Zipping lists together is a way to combine multiple lists into a single list:

    ```haskell
    > :t zip
    zip :: [a] -> [b] -> [(a, b)]

    > zip [1, 2, 3] ['a', 'b', 'c']
    [(1, 'a'), (2, 'b'), (3, 'c')]
    ```

- `zip` stops as soon as one list runs out, which means that one list can be infinite:

    ```haskell
    > zip (enumFrom 1) ['a', 'b', 'c']
    [(1, 'a'), (2, 'b'), (3, 'c')]
    ```

- `unzip` recovers the lists as they were originally:

    ```haskell
    > :t unzip
    unzip :: [(a, b)] -> ([a], [b])

    > unzip $ zip [1, 2, 3] ['a', 'b', 'c']
    ([1, 2, 3], "abc")
    ```

- `zipWith` applies a function to zip the values together:

    ```haskell
    > zipWith (+) [1, 2, 3] [10, 11, 12]
    [11, 13, 15]
    ```
