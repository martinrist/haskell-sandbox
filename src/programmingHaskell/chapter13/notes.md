# Chapter 13 - Building Projects

## 13.1 - Modules

- Haskell programs are organised into _modules_ that contain datatypes, typeclasses, instances etc.  They are similar to _namespaces_ in other languages.

- Common tools for working with Haskell programs are:
    - _Cabal_ - package manager
    - _Stack_ - build system


## 13.2 - Making packages with Stack

- _Cabal_ is the _Common Architecture for Building Applications and Libraries_.

- It is a _package manager_, which exists to help organise all the _dependencies_ of a package being developed.
    - _Cabal_ exists primarily to describe a single package, using a Cabal file that has a `.cabal` extension.

- _Stack_ is a cross-platform program for building Haskell projects:
    - Helps manage projects made up of single or multiple packages.
    - _Stack_ is built on top of _Cabal_, so still uses `.cabal` files.
    - Also relies on 'Long Term Support' snapshot of Haskell packages from [Stackage](https://stackage.org) that are guaranteed to work together, and not have conflicting dependencies.


## 13.3 - Working with a basic project

- To build project, run `stack build` (may need to run `stack setup` first to set up GHC.

- To start a REPL and load the module, run `stack ghci`.

- To execute the project directly from the command line, run `stack exec -- $PROJECT` after build.

- To find details of the paths being used by Stack, run `stack path`.

- Executable files are created using the `executable` stanza in the `.cabal` file:

    ```haskell
    executable hello
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
    ```


## 13.4 - Making our project a library

- To make the project a library, so it can be used by others, we need to add a `library` stanza to the `.cabal` file:

    ```haskell
    library
      hs-source-dirs:      src
      exposed-modules:     Hello
      build-depends:       base >= 4.7 && < 5
      default-language:    Haskell2010
    ```

- Then we would create a file `src/Hello.hs` with:

    ```haskell
    module Hello where

    -- Definitions go here
    ```

- We then _import_ the `Hello` module into the `Main` module using:

    ```haskell
    module Main where

    import Hello

    -- Use functions from `Hello` module here
    ```


## 13.5 - Module exports

- By default, all top-level bindings in a module are exported and can be imported by another module.

- To export a subset of the bindings, include them in parentheses after the `module` declaration:

    ```haskell
    module Hello
        ( sayHello, waveGoodbye )
        where

    sayHello :: IO ()
    -- This function is exposed

    waveGoodbye :: IO ()
    -- This function is exposed

    discussStuff :: IO ()
    -- This function is not visible outside the module
    ```


## 13.6 - More on importing modules

- In GHCI, we can use the `:browse` command to view the functions in a named module:

    ```haskell
    > :browse Data.Bool
    Data.Bool.bool :: a -> a -> Bool -> a
    -- and more
    ```

- To import specific functions from a module (either in a source file, or in GHCi), include the imported functions in parentheses as part of the `import` statement:

    ```haskell
    import Data.Bool (bool)
    ```

- By default, imported declarations do not need to be qualified, but we can use the `qualified` keyword to do this (e.g. if we need functions with the same name from different modules):

    ```haskell
    import qualified Data.Bool
    -- Functions in `Data.Bool` are accessed using (e.g.) `Data.Bool.bool`

    import qualified Data.List as L
    -- Functions in `Data.List` are accessed using (e.g.) `L.head`
    ```


## 13.7 - Making programs interactive

- The `main` method has a type signature of `IO ()` - the `IO` signified that it has side-effects.

- We can use _do_ syntax as syntactic sugar to sequence side-effects in a convenient manner:

    ```haskell
    main :: IO ()
    main = do
        name <- getLine
        putStrLn ("Hi " ++ name ++ "!")
    ```

- `getLine` has type `IO String` - it needs to perform I/O in order to obtain a `String`.

- The result of applying `<-` (_bind_) inside a `do` block to the `IO String` returned by `getLine` is to bind the returned value to `name`.

- `putStrLn` has type `String :: IO ()` - it takes the `String` value of `name` and outputs it.

- If we want to add a prompt for the name we need to use `putStr` and call `hSetBuffering stdout NoBuffering` to ensure the prompt is output immediately:

    ```haskell
    import System.IO


    main :: IO ()
    main = do
        hSetBuffering stdout NoBuffering
        putStr "Please enter your name: "
        name <- getLine
        putStrLn ("Hi " ++ name ++ "!")
    ```


## 13.8 - `do` syntax and `IO`

- `do`-syntax can make blocks more readable, but since they are syntactic sugar, they are not strictly required.

- `do`-syntax provides a way of naming values returned by monadic `IO` actions so that they can be used as inputs to actions that happen later in the program:

    ```haskell
    getAndConcat :: IO String
    getAndConcat = do
        x1 <- getLine
        x2 <- getLine
        return (x1 ++ x2)
    ```

- As above, `getLine` returns an `IO String`.  `<-` used inside the `do` block allows us to bind a name (here `x1` and `x2`) to the `a` of an `m a` value, where `m` is some monadic structure (here `IO`).

- `return` takes a value and wraps it in a monadic structure:

    ```haskell
    > :t return
    return :: Monad m => a -> m a

    > return 'a' :: Maybe Char
    Just 'a'

    > return 'a' :: Either Int Char
    Right 'a'

    > return 'a' :: [Char]
    "a"
    ```

- In the example above, we use `return` to make sure we return a value of type `IO String`.

- Take care with using `do`-blocks too much - they aren't needed in single-line expressions, where the bind operator `>>=` is preferred.
