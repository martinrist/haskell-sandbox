# Chapter 12 - Signalling Adversity

## 12.1 - Signalling Adversity

- Haskell uses explicit datatypes to signal when functions receive inputs that 'don't make sense' (for some reason).

- We can express that an item may be 'missing', without resorting to `null`.

- We can also epxress that a value can be 'either' one type or another, but not both, e.g. either a value or an error message.


## 12.2 - `Maybe` and `Nothing`

- The definition of `Maybe` in Prelude shows that it's fully polymorphic, and can represent the absence of a value:

    ```haskell
    data Maybe a = Nothing | Just a
    ```

- Example of the use of `Maybe`:

    ```haskell
    -- To be a total function, it needs to return a value for each input
    -- But perhaps we don't want to...
    ifEvenAdd2 :: Integer -> Integer
    ifEvenAdd2 n = if even n then n + 2 else n

    -- Changing the return type to `Maybe Integer` allows this to be modelled in the type system
    ifEvenAdd2' :: Integer -> Maybe Integer
    ifEvenAdd2' n = if even n then Just (n+2) else Nothing
    ```

- We can use `Maybe` to write _smart constructors_ that validate their arguments:

    ```haskell
    type Name = String
    type Age = Integer

    data Person = Person Name Age deriving Show

    mkPerson :: Name -> Age -> Maybe Person
    mkPerson name age
        | name /= "" && age >= 0 = Just $ Person name age
        | otherwise              = Nothing

    > mkPerson "" 160
    Nothing

    > mkPerson "Me" (-10)
    Nothing

    > mkPerson "David" 42
    Just (Person "David" 42)
    ```

- This is an improvement, but we may also want to know whether we had a bad age, or a bad name, or both.


## 12.3 - Bleating either

- The `Either` datatype allows us to represent a type that is either one thing or another:

    ```haskell
    data Either a b = Left a | Right b
    ```

- We can use this to solve the above problem, to see why a smart constructor fails:

    ```haskell
    data PersonInvalid = NameEmpty
                       | AgeTooLow
                       deriving (Eq, Show)

    mkPerson' :: Name -> Age -> Either PersonInvalid Person
    mkPerson' name age
        | name /= "" && age >= 0 = Right $ Person name age
        | name == ""             = Left NameEmpty
        | otherwise              = Left AgeTooLow
    ```

- Using `Left` as the data constructor for the invalid case is conventional in Haskell:
    - This is because the error / invalid case is the one that usually needs to stop processing.
    - The instance of `Functor` for `Either` will not map over the left type argument.

- If both error conditions arise, only one will be shown, but this can be improved by writing separate validation functions for each parameter:

    ```haskell
    ageOkay :: Age -> Either [PersonInvalid] Age
    ageOkay age = case age >= 0 of
                    True  -> Right age
                    False -> Left [AgeTooLow]

    nameOkay :: Name -> Either [PersonInvalid] Name
    nameOkay name = case name /= "" of
                        True  -> Right name
                        False -> Left [NameEmpty]
    ```

- We can then compose these validation functions together into an even smarter constructor which embeds the knowledge of how to combine the validation errors from each validation function:

    ```haskell
    type ValidatePerson a = Either [PersonInvalid] a

    mkPerson'' :: Name -> Age -> ValidatePerson Person
    mkPerson'' name age = mkPerson''' (nameOkay name) (ageOkay age)

    mkPerson''' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
    mkPerson''' (Right nameOK) (Right ageOk) = Right (Person nameOk ageOk)
    mkPerson''' (Left badName) (Left badAge) = Left (badName ++ badAge)
    mkPerson''' (Left badName) _             = Left badName
    mkPerson''' _              (Left badAge) = Left badAge
    ```


## 12.4 - Higher-kinded Types

- A _higher-kinded type_ is any type whose kind has a function arrow in it, and can be described as a type constructor, rather than a type constant:

    ```haskell
    > :k Maybe
    Maybe :: * -> *

    > :k Either
    Either :: * -> * -> *
    ```

- Just like with currying functions, applying a higher-kinded type to a type argument, 'reduces the kindedness by one', until it is fully-applied (`*`):

    ```haskell
    > :k Either
    Either :: * -> * -> *

    > :k Either Int
    Either Int :: * -> *

    > :k Either Int Bool
    Either Int Bool :: *
    ```

- To be precise, `*` is the kind of all standard _lifted_ types, whereas types with kind `#` are _unlifted_:
    - _Lifted types_ are those which _can_ be inhabited by _bottom_.
    - _Unlifted types_ are those which _cannot_ be inhabited by _bottom_.
    - All types that we can declare ourselves are _lifted_.
    - Unlifted types are often native machine types and raw pointers.

- `newtypes` are kind `*` but are unlifted relative to their contained type, because their representation is the same (i.e. the `newtype` is not creating any new pointer):
    - The newtype itself cannot be inhabited by _bottom_, only the type it contains.
    - Therefore the newtype itself is unlifted.
