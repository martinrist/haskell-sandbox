# Chapter 27 - Non-Strictness

## 27.1 - Laziness

- Haskell technically has 'non-strict' evaluation, not 'lazy':
    - A truly lazy language memoizes the results of all functions it evaluates.
    - This can use unacceptably-large amounts of memory, which is why Haskell doesn't use it.
    - However, the difference is not practically relevant.

- Most expressions are only reduced or evaluated when necessary:
    - When the evaluation process begins, a _thunk_ is created for each expression.
    - A _thunk_ is like a placeholder in the underlying graph of the program.
    - If the _thunk_ is needed, it's evaluated.
    - If not, it never gets reduced, and eventually gets garbage-collected.
    - As it's in a graph, a thunk can be shared across expressions.
    - Only needs to be evaluated once.


## 27.2 - Bottom

- Non-strictness is _defined_ by the ability to evaluate expressions that have bindings which have _bottom_ in them:
    - As long as the _bottom_ itself is never forced.
    - We can use _bottom_ as a means to understand non-strictness in Haskell.

- So, for example, the following expressions only work in a non-strict language, otherwise the `undefined` would be evaluated:

    ```haskell
    > fst (1, undefined)
    1

    > snd (undefined, 2)
    2
    ```


## 27.3 - Outside in, inside out

- Strict languages evaluate _inside out_, i.e. evaluating the innermost parts of expressions first:
    - Non-strict languages evaluate _outside in_, i.e. evluating the outer parts of the expression first, and only evaluating the inner parts if needed.

- Consider evaluation of the following:

    ```haskell
    possiblyKaboom =
        \f -> f fst snd (0, undefined)

    true :: a -> a -> a
    true = \a -> (\b -> a)

    false :: a -> a -> a
    false = \a -> (\b -> b)

    -- with `true` the `undefined` is never evaluated
    > possiblyKaboom true
    > (\f -> f fst snd (0, undefined)) (\a -> (\b -> a))
    > (\a -> (\b -> a)) fst snd (0, undefined)
    > (\b -> fst) snd (0, undefined)
    > fst (0, undefined)
    > 0

    -- with `false`, the `undefined` is evaluated
    > possiblyKaboom false
    > (\f -> f fst snd (-, undefined)) (\a -> (\b -> b))
    > (\a -> (\b -> b)) fst snd (0, undefined)
    > (\b -> b) snd (0, undefined)
    > snd (0, undefined)
    > CallStack (from HasCallStack):
      error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
        undefined, called at examples.hs:12:25 in main:Main
    ```

- The next exmaple is similar - it only blows up if passed `False`:

    ```haskell
    possiblyKaboom b =
        case b of 
            True  -> fst tup
            False -> snd tup
        where tup = (0, undefined)
    ```


## 27.4 - What does the other way look like?

- In _strict_ languages, you can't usually bind a computation to a name without having already done all the work to construct and evaluate it fully.

- Another example of somethhing that wouldn't work in a strict language:

    ```haskell
    hypo :: IO ()
    hypo = do
        let x :: Int
            x = undefined
        s <- getLine
        case s of
             "hi" -> print x
             _    -> putStrLn "hello"
    ```

- In a strict language, this would fail at the `x = undefined` line:
    - In Haskell, it will work if you type in anything other than `hi`, as `x` is never evaluated.

- We can add strictness by replacing `s` in the `case` with ``x `seq` s``:

    ```haskell
    hypo' :: IO ()
    hypo' = do
        let x :: Int
            x = undefined
        s <- getLine
        case (x `seq` s) of
             "hi" -> print x
             _    -> putStrLn "hello"
    ```

- `seq :: a -> b -> b` forces evaluation of the first argument any time the second argument is evaluated:
    - In the above example, it effectively establishes a link between `x` and `s` in the expression graph for the program.

- `seq` actually evaluates up to _weak head normal form (WHNF)_, which stops at the first data constructor or lambda it encounters:

    ```haskell
    > let dc = (,) undefined undefined
    > let noDc = undefined
    > let lam = \_ -> undefined

    -- Evaluation stops at the (,) data constructor
    > dc `seq` 1
    1

    -- Attempts to evaluate `undefined`
    > noDc `seq` 1
    *** Exception: Prelude.undefined

    -- Evaluation stops at the lambda
    > lam `seq` 1
    1
    ```

- The forcing behaviour we see with `seq` also happens when casing or pattern-matching on something.


## 27.5 - Call by name, call by need

- Various terminology can be used for the way we call things:
    - _Call by value_: argument expressions evaluated before entering function.  _Strict_, or _inside-out_ evaluation.
    - _Call by name_: expressions can be arguments to a function without having been evaluated.  _Non-strict_, or _outside-in_ evaluation.
    - _Call by need_: as _call by name_, but expressions are evaluated only once.  Only happens some of the time in GHC Haskell (e.g. when an expression isn't a lambda with arguments and a name).  Also _non-strict_, _outside-in_.



## 27.6 - Non-strict evaluation changes what we can do

- Sometimes, with non-strict evaluation, we can do things that we wouldn't otherwise be able to do:

    ```haskell
    -- This works in both strict and non-strict languages
    > tail [1, 2, 3]
    [2, 3]

    -- This only works in non-strict languages
    > tail [undefined, 2, 3]
    [2, 3]

    -- This doesn't work, because `sort` needs to traverse the whole list
    > head $ sort [1, 2, 3, undefined]
    *** Exception: Prelude.undefined
    ```


## 27.7 - Thunk life

- A _thunk_ is a computation that has not yet been evaluated up to WHNF.

- We can use `sprint` in GHCi to show what's been evaluated and what hasn't:

    ```haskell
    -- GHCi opportunistically evaluates values which are merely
    -- data constructors (and therefore constant, since they're fully
    -- applied)
    > let myList = [1, 2, 3] :: [Integer]
    > :sprint myList
    myList = [1, 2, 3]

    -- However, if we make `myList` more polymorphic
    -- there are no data constructors to evaluate:
    > let myList = [1, 2, 3]
    > :sprint myList
    myList = _

    -- sprint stops evaluating when it hits a computation
    > let myList = [1, 2, id 1] :: [Integer]
    > :sprint myList
    myList = [1, 2, _]

    -- Here, the outermost term is a function (++)
    -- so the whole expression is thunked
    > let myList = [1, 2, id 1] :: [Integer]
    > let myList' = myList ++ undefined
    > :sprint myList'
    myList' = _
    ```


## 27.8 - Sharing is caring

- When a computation is named, the results of evaluating it can be shared between all references to it, without needing to re-evaluate it.

- GHC switched between sharing (call-by-need) and not (call-by-name) based on necessity and what it thinks will produce faster code.

- The `base` library has a module `Debug.Trace` that has functions useful for observing sharing, using the `trace :: String -> a -> a` method:

    ```haskell
    > import Debug.Trace
    > let a = trace "a" 1
    > let b = trace "b" 2

    -- `trace` shows that `b` is evaluated first
    -- although we shouldn't rely on this evaluation order
    > a + b
    b
    a
    3
    ```

- `trace` can show examples of sharing:

    ```haskell
    inc = (+1)

    twice = inc . inc

    howManyTimes =
        inc (trace "I got eval'd" (1 + 1))
            + twice (trace "I got eval'd" (1 + 1))

    howManyTimes' =
        let onePlusOne = trace "I got eval'd" (1 + 1)
        in inc onePlusOne + twice onePlusOne

    -- `howManyTimes` shows non-sharing
    > howManyTimes
    I got eval'd
    I got eval'd
    7

    -- `howManyTimes'` only evaluates the trace once
    > howManyTimes'
    I got eval'd
    7
    ```


## 27.9 - Refutable and irrefutable patterns

- An _irrefutable pattern_ is one which will never fail to match, whereas a _refutable pattern_ is one which has potential failures, e.g.:

    ```haskell
    -- `refutable` is refutable because each case is individually
    -- refutable (i.e. might not match)
    refutable :: Bool -> Bool
    refutable True = False
    refutable False = True

    -- `irrefutable` is irrefutable because it only has one
    -- match, which can never fail
    irrefutable :: Bool -> Bool
    irrefutable x = not x

    -- `oneOfEach` has both (first pattern is refutable, second isn't) 
    oneOfEach :: Bool -> Bool
    oneOfEach True = False
    oneOfEach _ = True
    ```

- We can make a pattern match lazy by prefixing with a `~`:

    ```haskell
    strictPattern :: (a, b) -> String
    strictPattern (a, b) = const "Cousin It" a

    lazyPattern :: (a, b) -> String
    lazyPattern ~(a, b) = const "Cousin It" a

    > strictPattern undefined
    *** Exception: Prelude.undefined

    > lazyPattern undefined
    "Cousin It"
    ```


## 27.10 - Bang-patterns

- Sometimes we want to evaluate a function argument whether we use it or not.

- We can do this with `seq`:

    ```haskell
    -- `b` is never evaluated
    doesntEval :: Bool -> Int
    doesntEval b = 1

    -- `b` is evaluated, because of the `seq`
    manualSeq :: Bool -> Int
    manualSeq b = b `seq` 1
    ```

- Alternatively, we can use a _bang-pattern_ on `b`:

    ```haskell
    -- `b` is always evaluated
    banging :: Bool -> Int
    banging !b = 1
    ```

- We can specify a data constructor argument as strict using `!`:

    ```haskell
    data Foo = Foo Int !Int

    first (Foo x _) = x
    second (Foo _ y) = y

    -- `second` doesn't evaluate the unused non-strict argument
    > second (Foo undefined 1)
    1

    -- But the second (strict) argument can't be `undefined`
    > second (Foo 1 undefined)
    *** Exception: Prelude.undefined
    ```


## 27.11 - `Strict` and `StrictData`

- We can use the `Strict` and `StrictData` language pragmas to switch to strict mode for a module.

- Works for GHC 8.0 or later.

