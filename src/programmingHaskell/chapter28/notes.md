# Chapter 28 - Basic Libraries

## 28.2 - Benchmarking with Criterion

- We can use the [`criterion`](http://hackage.haskell.org/package/criterion) library to benchmark our code.

- We also need to compile code with `-O` or `-O2` in the GHC build flags:

    ```haskell
    -- To run ghc manually with stack
    stack ghc -- -O2 bench.hs
    ```

- Here's an example of trying to write a total replacement for `!!`:

    ```haskell
    infixl 9 !?
    _      !? n | n < 0 = Nothing
    []     !? _         = Nothing
    (x:_)  !? 0         = Just x
    (_:xs) !? n         = xs !? (n-1)

    myList :: [Int]
    myList = [1..9999]

    main :: IO ()
    main = defaultMain
        [ bench "index list 9999"
          $ whnf (myList !!) 9998
        , bench "index list maybe index 9999"
          $ whnf (myList !?) 9998
        ]
    ```

- `defaultMain` is defined by Criterion as `[Benchmark] -> IO ()`:
    - Benchmarks are created with `bench :: String -> Benchmarkable -> Benchmark`
    - `whnf :: (a -> b) -> a -> Benchmarkable` takes a function so that its result isn't shared and can be re-run.

- Our initial run shows that our first implementation is much slower than `!!`:

    ```haskell
    $ stack ghc -- -O2 examples.hs

    $ ./examples
    benchmarking index list 9999
    time                 20.88 μs   (20.73 μs .. 21.04 μs)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 21.03 μs   (20.89 μs .. 21.21 μs)
    std dev              535.1 ns   (381.8 ns .. 729.5 ns)
    variance introduced by outliers: 26% (moderately inflated)

    benchmarking index list maybe index 9999
    time                 156.3 μs   (154.7 μs .. 158.0 μs)
                         0.999 R²   (0.998 R² .. 0.999 R²)
    mean                 158.2 μs   (156.0 μs .. 164.4 μs)
    std dev              12.07 μs   (4.856 μs .. 21.97 μs)
    variance introduced by outliers: 70% (severely inflated)
    ```

- In this case, adding a type signature to force `(!?) :: [a] -> Int -> Maybe a` means that the second argument is forced to a more efficient `Int` (versus the default `Integer`):
    - This makes the performance close to the original.


## 28.5 - `Map`

- `Data.Map.Map` is the basic associative data structure:

    ```haskell
    data Map k a
        = Bin !Size !k a !(Map k a) !(Map k a)
        | Tip

    type Size = Int
    ```

- Maps are much faster at lookup than just a pair of tuples (e.g. `Map String Int` vs `[(String, Int)]`.



## 28.6 - `Set`

- `Set` is like `Map` but without the _value_ part:
    - The keys are effectively the values.
    - Cannot have duplicate keys



## 28.7 - `Sequence`

- `Sequence` appends cheaply onto the front and back:
    - Avoids a common list problem where you can only `cons` onto a list cheaply at the front.

- Updates (cons and append) to both ends of the data structure, and concatenation are fast.



## 28.9 - String types

- Although `String` is a type alias for `[Char]`, although under the hood it's more complicated than that.

- Since a `String` is a list it can be infinite, and the memory usage for very large strings can get out of control rapidly.

- Character-by-character indexing into large strings can also be slow.

- The `text` library contains the `Text` type which allows for more efficient storage of plain text.

- `Text` also allows for more efficient indexing into the text.

- However, `Text` is UTF-16 encoded, as opposed to the more common UTF-8.

- `ByteString`s are seqiences of bytes represented as a vector of `Word8` values.

- `ByteString` is easy to use via the `OverloadedStrings` pragma.

