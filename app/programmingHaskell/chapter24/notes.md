# Chapter 24 - Parser Combinators

## 24.1 - Example of parsing

- Parsing in computer science is similar to parsing sentences, e.g.:

    ```
    Boy plays with dog


                   S(entence)
                  / \
     Boy (subject)   plays (verb)
                      \
                       with (preposition)
                        \
                         dog (object)
    ```


## 24.3 - Understanding the parsing process

- A _parser_ is a function that takes textual input (e.g. `String`, `ByteString` or `Text`) and returns some structure as output:
    - Output may be (for example) a tree, or an indexed map of locations in the parsed data.

- A _parser combinator_ is a higher-order function that takes parsers as input and returns na new parser as output.

- Among other things, parser combinators allow for gluing together parsers in a modular fashion to parse data according to complex rules.

- As an example, use the `trifecta` parsing library:

    ```haskell
    import Text.Trifecta

    stop :: Parser a
    stop = unexpected "stop"
    ```

- Let's say we have a function that only parses one character, then sequence that with `stop` to die:

    ```haskell
    -- read a single character '1'
    one = char '1'

    -- read a single character '1' then die
    one' = one >> stop
    ```

- To do the sequencing, we're using the sequencing operator `>>` from `Monad`, which sequentially composes two actions, discarding any value produced by the first:

    ```haskell
    (>>) :: Monad m => m a -> m b -> m b

    > [1, 2, 3] >> "ab"
    "ababab"

    > Just 1 >> Just 2
    Just 2

    > Nothing >> Just 2
    Nothing

    > Just 1 >> Nothing
    Nothing
    ```

- The use of `>>` when composing `one` and `stop` means that whatever `one` returns gets thrown away:
    - However, any _effect_ the `one` action had on the monadic context remains.
    - In this case, the effect is 'moving the cursor along the input string'.
    - Another possible effect is causing the parser to fail.

- What's happening here is a bit like the `State` monad, but with the added possibility of failure:

    ```haskell
    newtype Reader r a = Reader { runReader :: r -> a }

    newtype State s a  = State  { runState  :: s -> (a, s) }

    type Parser a      =                  String -> Maybe (a, String)

    -- Await a string value
    -- Produce a result which may or may not succeed (`Nothing` = failed)
    -- Return a tuple of the value parsed, and the remainder of the String
    ```

- Looking at the underlying pattern of a parsing function such as `char` (not its actual implementation) we can see the State-like behaviour:

    ```haskell
    -- Note that this doesn't include the possibility of failure using `Maybe`
    char :: Char -> Parser Char
    char c =
        Parser $ \s ->
            case s of
                (x:xs) -> if c == x
                        then [(c, xs)]
                        else []
                _      -> []
    ```

- Using `Control.Monad.Trans.State.StateT` we can see the effect of putting things into the state monad and passing them along:

    ```haskell
    get :: Monad m => StateT s m s
    put :: Monad m => s -> StateT s m ()
    runStateT :: StateT s m a -> s -> m (a, s)

    -- Put '8' into the state and run it
    > runStateT (put 8) 42
    ((), 8)

    -- Get the value of the state into the result 'a'
    > runStateT get 8
    (8, 8)

    -- Start composing together a `put` and a `get`
    > runStateT (put 1 >> get) 42
    (1, 1)

    -- Put a value (2) into the state and return another (9001) as the return value
    > runStateT (put 2 >> return 9001) 10
    (9001, 2)
    ```

- We can sequence two parsers together using `>>`:

    ```haskell
    -- Reads a '1' then a '2'
    oneTwo = char '1' >> char '2'

    -- Reads a '1' then a '2' then dies
    oneTwoDie = oneTwo >> stop

    -- Test out a parser
    testParse :: Parser Char -> IO ()
    testParse p = print $ parseString p mempty "123"
    ```

- Various outputs from running the above parsers using `testParse`:

    ```haskell
    -- Fail immediately before consuming any input
    > testParse stop
    Failure (interactive):1:1: error: unexpected
        stop
    123<EOF>
    ^

    -- `one` parses a single character '1'.
    -- `parseString` drops the rest of the output
    > testParse one
    Success '1'

    -- `one'` parses the initial '1'
    -- then drops it because we sequenced with `stop`
    > testParse one'
    Failure (interactive):1:2: error: unexpected
        stop
    123<EOF>
     ^

    -- `oneTwo` parses the intial '1' then '2',
    -- but ony returns the last thing that parsed (the '2')
    > testParse oneTwo
    Success '2'

    -- `oneTwo'` does the same as `oneTwo` but then fails
    > testParse oneTwo'
    Failure (interactive):1:3: error: unexpected
        stop
    123<EOF>
      ^
    ```
