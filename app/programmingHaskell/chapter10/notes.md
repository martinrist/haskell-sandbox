# Chapter 10 - Folding Lists

## 10.2 - Folds

- `foldr` is the most commonly used fold used for lists:

    ```haskell
    -- GHC 7.8 and earlier
    foldr :: (a -> b -> b) -> b -> [a] -> b

    -- GHC 7.10 and later (using Foldable)
    foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
    ```

- Notice the following parallel between `map` and `foldr`.  Where `map` applies a function to each member of a list and returns a list, a fold replaces the cons constructors with the function and reduces the list:

    ```haskell
       map   (+1) [1, 2, 3]
    => map   (+1) 1 :      2 :      3 : []
    =>       (+1) 1 : (+1) 2 : (+1) 3 : []
    =>            2 :      3 :      4 : []
    =>            [2, 3, 4]

       foldr (+) 0 [1, 2, 3]
    => foldr (+) 0 (1 :  2 :   3 : [])
    =>              1 + (2 +  (3 + 0))
    =>              6
    ```


## 10.3 - Recursive patterns

- Consider various recursive implementations of a number of functions:

    ```haskell
    sum :: [Integer] -> Integer
    sum []     = 0
    sum (x:xs) = x + sum xs

    length :: [a] -> Integer
    length []     = 0
    length (_:xs) = 1 + length xs

    product :: [Integer] -> Integer
    product []     = 1
    product (x:xs) = x * product xs

    concat :: [[a]] -> [a]
    concat []     = []
    concat (x:xs) = x ++ concat xs
    ```

    The base case is the identity for the function (e.g. `0` for `(+)`, `[]` for `concat`), and the recursive pattern associates to the right.


## 10.4 - Fold right

- `foldr` is called the _right fold_ because the fold is right associative:

    ```haskell
    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr _ z []     = z
    foldr f z (x:xs) = f x (foldr f z xs)
    ```

- We can think about Haskell evaluation in terms of rewriting

    ```haskell
       foldr (+) 0 [1, 2, 3]
    => (+) 1 (foldr (+) 0 [2, 3])
    => (+) 1 ((+) 2 (foldr (+) 0 [3]))
    => (+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [])))
    => (+) 1 ((+) 2 ((+) 3 0))
    ```

    This is now fully-rewritten, and you can see the right-associativity in the positioning of the parentheses.  Then we evaluate according to the normal rules of precedence:

    ```haskell
       (+) 1 ((+) 2 ((+) 3 0))
    => (+) 1 ((+) 2 3)
    => (+) 1 5
    => 6
    ```

- Folding occurs in two stages:
    - _Traversal_ - where the fold recurses over the spine.
    - _Folding_ - evaluation or reduction of the folding funciton applied to the values.

- The difference between left and right folds is in the association of the folding functions, and therefore the direction in which folding or reduction proceeds.


## 10.5 - Fold left

- Left folds traverse the spine in the same direction as right folds, but the folding process is _left associative_ and proceeds in the opposite direction:

    ```haskell
    foldl :: (b -> a -> b) -> b -> [a] -> [b]
    foldl f acc []     = acc
    foldl f acc (x:xs) = foldl f (f acc x) xs
    ```

    Note that the signature of the folding function `f` is `b -> a -> b`, whereas for `foldr` it was `a -> b -> b`.

- Example evaluation, showing the left-associativity of `foldl`:

    ```haskell

       foldl (+) 0 [1, 2, 3]
    => foldl (+) (0 + 1) [2, 3]
    => foldl (+) ((0 + 1) + 2) [3]
    => foldl (+) (((0 + 1) + 2) + 3) []
    =>           (((0 + 1) + 2) + 3)
    =>           6
    ```

- Note that `foldl` begins the reduction process by adding the `acc` (accumulator) value to the start of the list, whereas `foldr` adds it to the end.

- The difference between `foldl` and `foldr` is harder to see with associative functions, because they come up with the same result.  However, with a non-associative function like `(^)`:

    ```haskell
    > foldr (^) 2 [1..3]
    1                       -- i.e. (1 ^ (2 ^ (3 ^ 2))) = (1 ^ (2 ^ 9)) = 1 ^ 512 = 1

    > foldl (^) 2 [1..3]
    64                      -- ie (((2 ^ 1) ^ 2) ^ 3) = (2 ^ 2) ^ 3 = 4 ^ 3 = 64
    ```


## 10.6 - How to write fold functions

- First step is to consider the start value for the fold - usually the identity for the folding function - e.g. `0` for `(+)`.

- Next step is to consider the arguments - `a` and `b`, where `a` is one of the elements of the list, and `b` is either the start value, or the value accumulated by the list being processed.