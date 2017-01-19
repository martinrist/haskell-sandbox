# Chapter 14 - Testing

## 14.3 - Conventional Testing with Hspec

- [HSpec](http://hackage.haskell.org/package/hspec) is a library used to support unit testing in Haskell.

- To use:

    ```haskell
    import Test.Hspec

    main :: IO ()
    main = hspec $ do
        describe "Addition" $ do
            it "1 + 1 is greater than 1" $ do
                (1 + 1) > 1 `shouldBe` True
    ```

- Sample output when run:

    ```haskell
    > main

    Addition
      1 + 1 is greater than 1

    Finished in 0.0005 seconds
    1 examples, 0 failures
    ```

- `shouldBe` is like a version of `==` with extra features for Hspec:

    ```haskell
    > :t shouldBe
    shouldBe :: (Eq a, Show a) => a -> a -> Expectation
    ```


## 14.4 - QuickCheck

- QuickCheck allows us to do _property testing_ - asserting properties that should hold.

- QuickCheck is integrated with HSpec, so to make a simple property assertion:

    ```haskell
    import Test.QuickCheck

    main :: IO ()
    main = hspec $ do
        describe "Addition" $ do
            it "x + 1 is always greater than x" $ do
                property $ \x -> x + 1 > (x :: Int)
    ```

- We need to assert the type of `x` in the `property`, otherwise we'll get an error.

- QuickCheck relies on a typeclass `Arbitrary` and a newtype called `Gen` for generating random data to be used for testing:

    ```haskell
    > :t arbitrary
    arbitrary :: Arbitrary a => Gen a

    > :i Arbitrary
    class Arbitrary a where
        arbitrary :: Gen a
        shrink :: a -> [a]
    ```

- When using `arbitrary`, we need to specify the type in order to dispatch to the right typeclass instance:

    ```haskell
    > genInt = arbitrary :: Gen Int
    ```

- Then we can call `sample` to retrieve a random value, or `sample'` to return a list:

    ```haskell
    > :t sample
    sample :: Show a => Gen a -> IO ()   -- IO () because it uses randomness

    > sample (arbitrary :: Gen Int)
    0
    2
    -1
    6
    -2
    0
    1
    -6
    -4
    17
    19
    -- (for example)

    > sample' (arbitrary :: Gen Int)
    [0,-1,0,-6,5,8,-11,5,-4,6,15]
    ```

- Can specify our own generator for a type:

    ```haskell
    -- `return` wraps the `Int` in the `Gen` monad
    trivialInt :: Gen Int
    trivialInt = return 1

    > sample' trivialInt
    [1,1,1,1,1,1,1,1,1,1,1]
    ```

- To randomly select from various elements, use `elements`:

    ```haskell
    -- Uniform distribution
    oneThroughThree :: Gen Int
    oneThroughThree = elements [1, 2, 3]

    -- Non-uniform distribution
    oneThroughThree' :: Gen Int
    oneThroughThree' = elements [1, 2, 2, 2, 2, 3]
    > sample' oneThroughThree
    [3,3,3,3,1,2,2,2,1,3,3]
    ```

- An alternative is `choose`, which returns a generator which returns values in a range:

    ```haskell
    oneThroughTen :: Gen Int
    oneThroughTen = choose (1, 10)

    > sample' oneThroughTen
    [6,3,8,7,5,2,4,5,10,4,7]
    ```

- We can create and use tuple generators as follows:

    ```haskell
    genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
    genTuple = do
        a <- arbitrary
        b <- arbitrary
        return (a, b)

    > intCharGen = genTuple :: Gen (Int, Char)
    > sample' intCharGen
    [(0,'\169'),(0,','),(1,'\222'),(2,'\ETX'),(7,'Q'),(9,'\STX'),(-7,'}'),(5,'\EM'),(-3,'+'),(-9,'\199'),(3,'b')]
    ```

- We can adjust the frequency of values using `frequency`, e.g. to create a `Maybe a` generator that returns more `Just a`'s:

    ```haskell
    genMaybe :: Arbitrary a => Gen (Maybe a)
    genMaybe = do
        a <- arbitrary
        frequency [ (1, return Nothing),
                    (3, return (Just a))]

    > sample' (genMaybe :: Gen (Maybe Int))
    [Just 0,Just 0,Just 1,Just 1,Just (-1),Just (-3),Just (-9),Just 11,Just 2,Nothing,Just (-13)]
    ```

- To use QuickCheck without Hspec, just write a function for the property, then call `quickCheck`:

    ```haskell
    prop_additionGreater :: Int -> Bool
    prop_additionGreater x = x + 1 > x

    runQc :: IO ()
    runQc = quickCheck prop_additionGreater
    ```


## 14.6 - Kicking around QuickCheck

- To enable our type to be used with QuickCheck it needs to have an instance for `Arbitrary` 

    ```haskell
    data Trivial = Trivial deriving (Eq, Show)

    trivialGen :: Gen Trivial
    trivialGen = return Trivial

    instance Arbitrary Trivial where
        arbitrary = trivialGen
    ```

- For a slightly more complicated example, consider an `Identity` type:

    ```haskell
    data Identity a = Identity a deriving (Eq, Show)

    identityGen :: Arbitrary a => Gen (Identity a)
    identityGen = do
        a <- arbitrary
        return (Identity a)

    -- We can make `identityGen` the default generator for the `Identity` type:
    instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = identityGen

    -- Now we can get a generator for `Identity Int`...
    identityIntGen :: Gen (Identity Int)
    identityIntGen = arbitrary

    -- ... then sample it
    > sample' identityIntGen
    [Identity 0, Identity 1, Identity (-4) ...]
    ```

- `Arbitrary` instances for product types - just pick an arbitrary value for each of the type arguments:

    ```haskell
    data Pair a b = Pair a b deriving (Eq, Show)

    pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
    pairGen = do
        a <- arbitrary
        b <- arbitrary
        return (Pair a b)

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
        arbitrary = pairGen

    > pairIntStringGen = arbitrary :: Gen (Pair Int String)
    > sample' pairIntStringGen
    [ Pair 0 "", Pair 2 "", Pair 0 ";@yf475d", Pair (-4) "\229rC\205S\SUB\CAN" ...]
    ```

- `Arbitrary` instances for sum types are more interesting because you need to choose between the two possibilities, using `oneof` to create a `Gen a` from a list of `Gen a` (with uniform distribution):

    ```haskell
    data Sum a b = First a | Second b deriving (Eq, Show)

    sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
    sumGenEqual = do
        a <- arbitrary
        b <- arbitrary
        oneOf [return $ First a,
               return $ Second b]

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
        arbitrary = sumGenEqual
    ```

- For non-uniform distribution, we can use `frequency` - e.g. from the `Arbitrary` instance for `Maybe`:

    ```haskell
    instance Arbitrary a => Arbitrary (Maybe a) where
        arbitrary =
            frequency [(1, return Nothing),
                       (3, liftM Just arbitrary)]
    ```
