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

- 


    ```haskell
    sum :: [Integer] -> Integer
    sum []     = 0
    sum (x:xs) = x + sum xs

    length :: [a] -> Integer
    length []     = 0
    length (_:xs) = 1 + length xs

    product :: [Integer] -> Integer
    product []    = 1
    product (x:xs