# Chapter 23 - State

## 23.3 - Random numbers

- The `System.Random` library is designed to generate pseudo-random values, through a seed value or from the system-initialised generator:
    - `StdGen` is a datatype that is a product of two `Int32` values.
    - These are the seed values to generate the next random value.
    - `mkStdGen :: Int -> StdGen` creates a `StdGen` from an `Int` seed.
    - `next :: g -> (Int, g)` takes a generator, and returns a tuple of the generated value and the next generator to use.
    - `random :: (RandomGen g, Random a) => g -> (a, g)` is like `next` but generates non-numeric random values whose range is determined by the type.

- Example of using `next`:

    ```haskell
    > mkStdGen 0
    1 1

    > :t mkStdGen 0
    mkStdGen 0 :: StdGen

    > let sg = mkStdGen 0
    > next sg
    (2147482884,40014 40692)

    -- Calling `next sg` again gives the same result:
    > next sg
    (2147482884,40014 40692)

    -- This is the next value
    > fst $ next sg
    2147482884

    -- This is the next generator to use
    > snd $ next sg
    40014 40692

    -- This is the next random value
    > next (snd (next sg))
    (2092764894,1601120196 1655838864)
    ```

- Example using `random`:

    ```haskell
    -- `random sg` gives a polymorphic type
    > :t random sg
    random sg :: Random a => (a, StdGen)

    -- By specifying the type to use, we can generate non-`Int` random values:
    > fst $ random sg :: Int
    9106162675347844341

    > fst $ random sg :: Double
    0.9871468153391151
    ```

- Chaining the state (e.g. extracting the `StdGen` using `snd`) gets tedious - this is the point of the `State` monad.



## 23.4 - The `State` newtype

- `State` is defined in a newtype (like `Reader`):

    ```haskell
    newtype State s a =
        State { runState :: s -> (a, s) }

    -- Notice the similarity to the `Reader` newtype:
    newtype Reader r a =
        Reader { runReader :: r -> a }
    ```

- The `State` data constructor and `runState` record accessor are the way we put a value in and take a value out of `State`:

    ```haskell
    State :: (s -> (a, s)) -> State s a

    runState :: State s a -> s -> (a, s)
    ```


## 23.5 - Throw down

- Example of generating rolls from dice:

    ```haskell
    data Die =
        DieOne
      | DieTwo
      | DieThree
      | DieFour
      | DieFive
      | DieSix
      deriving (Eq, Show)

    intToDie :: Int -> Die
    intToDie n =
        case n of
            1 -> DieOne
            2 -> DieTwo
            ...
    ```

- The 'hard' way of doing this is to manually chain the state:

    ```haskell
    roll3Dice :: (Die, Die, Die)
    roll3Dice = do
        let s = mkStdGen 0
            (d1, s1) = randomR (1, 6) s
            (d2, s2) = randomR (1, 6) s1
            (d3, s3) = randomR (1, 6) s2
        (intToDie d1, intToDie d2, intToDie d3)
    ```

- 

