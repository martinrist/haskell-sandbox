# Chapter 7 - More functional patterns

## 7.2 - Arguments and parameters

- Functions may appear to have multiple parameters, but because of currying, all functions take one argument and return one result.

- All Haskell values can be arguments to functions - a value that can be used as an argument to a function is called a _first-class_ value.  This includes functions.

- Applying a function binds its parameters to values - type parameters become bound to a type, and function variables are bound to a value.

- Binding also occurs in things like `let` expressions and `where` clauses:

    ```haskell
    bindExp :: Integer -> String
    bindExp x = let y = 5 in
                    "the integer was: " ++ show x
                    ++ " and y was: " ++ show y

    > bindExp 2
    "the integer was: 2 and y was: 5"

    > bindExp 3
    "the integer was: 3 and y was: 5"
    ```

- Function arguments may not be visible if they have been _shadowed_ by an inner `let`:

    ```haskell
    bindExp :: Integer -> String
    bindExp x = let x = 10; y = 5 in
                    "the integer was: " ++ show x
                    ++ " and y was: " ++ show y

    > bindExp 2
    "the integer was: 10 and y was: 5"

    > bindExp 3
    "the integer was: 10 and y was: 5"
    ```

- Haskell is _lexically-scoped_ - resolving the value for a named entity depends on the location in the code and the lexical context.


## 7.3 - Anonymous functions

- Anonymous functions are defined using a lambda-style syntax:

    ```haskell
    > (\x -> x * 3) 10
    30

    > let triple = (\x -> x * 3)
    > triple 5
    15
    ```
