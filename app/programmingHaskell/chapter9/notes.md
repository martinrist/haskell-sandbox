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
