# Chapter 21 - Traversable

## 21.1 - `Traversable` motivation

- `Functor` gives us a way to transform values embedded in a strucutre.

- `Applicative` is a _monoidal functor_, which gives us a way to transform values embedded in a structure, using a function that is also embedded in a structure:

- `Foldable` gives us a way to process values embedded in a structure as if they exist in sequential order (like they were a list).

- However, things like `Functor` and `Foldable` are not sufficient to describe all the possible ways of 'traversing' over a list.

- For example, consider the following test for negative numbers:

    ```haskell
    deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
    deleteIfNegative x = if x < 0 then Nothing else Just x
    ```

- Say we want to implement a function `rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]` which:
    - Gives back the original list wrapped in `Just` if there are no negatives in it
    - otherwise gives back `Nothing`.

- Trying to use `Functor`, doesn't work because we have no way of turning the resulting `[Maybe a]` into a `Maybe [a]`:

    ```haskell
    > let testList = [-5, 3, 2, -1, 0]
    > fmap deleteIfNegative testList
    [Nothing, Just 3, Just 2, Nothing, Just 0]
    ```

- `Foldable` replaces the structure of the original list with that of whatever `Monoid` we use for folding, and there's no way of twisting that into giving either the original list or `Nothing`.

- This is the motiviation for `Traversable`:
    - depends on `Applicative` and thus `Functor`
    - is also superclassed by `Foldable`



## 21.2 - The `Traversable` typeclass definition

- `Traversable` has the following typeclass definition:

    ```haskell
    class (Functor t, Foldable t) => Traversable (t :: * -> *) where
        traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
        sequenceA :: Applicative f => t (f a) -> f (t a)
        -- More functions
        {-# MINIMAL traverse | sequenceA #-}
    ```

- A minimal instance defines either `traverse` or `sequenceA`, but they can both be defined if we have more information about our datatype that enables a better implementation.



## 21.3 - `sequenceA`

- `sequenceA` evaluates each action in the structure from left to right, and collects the results:

    ```haskell
    sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
    ```

- Effectively `sequenceA` 'flips' two contexts or structures around, e.g. from a list of `Maybe`s to a `Maybe` of a list:

    ```haskell
    > sequenceA [Just 1, Just 2, Just 3]
    Just [1, 2, 3]

    > sequenceA [Just 1, Nothing, Just 3]
    Nothing

    -- Works the other way as well
    > sequenceA $ Just [1, 2, 3]
    [Just 1, Just 2, Just 3]
    ```

- As an alternative way of processing a list of `Maybe`s, consider using `Data.Maybe.catMaybe`:

    ```haskell
    > import Data.Maybe
    > :t catMaybes
    catMaybes :: [Maybe a] -> [a]

    > catMaybes [Just 1, Nothing, Just 3]
    [1, 3]

    > catMaybes [Nothing, Nothing, Nothing]
    []
    ```


## 21.4 - `traverse`

- `traverse` maps each element of a structure to an action, evaluates those actions from left to right, and collects the results:

    ```haskell
    traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
    ```

- Compare the type signature of `traverse` with `fmap` and `=<<` (a.k.a. `flip bind`):

    ```haskell
    fmap     :: Functor f                      => (a -> b)   -> f a -> f b
    (=<<)    :: Monad   m                      => (a -> m b) -> m a -> m b
    traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
    ```

- With `traverse`:
    - we're still mapping a function over embedded values, like `fmap`
    - the mapping function is providing extra structure, like `=<<`
    - _however_, the structure added by this function (`f`) can be different from the structure mapped over (`t`)

- For example, with `f` -> `Maybe` and `t` -> [], we get our desired implementation of `rejectWithNegatives` from above:

    ```haskell
    > traverse deleteIfNegative [-1, 2, -3, 4]
    Nothing

    > traverse deleteIfNegative [1, 2, 3, 4]
    Just [1, 2,3, 4]
    ```
- `traverse` is just `sequenceA . fmap`



## 21.5 - Uses of `Traversable`

- Any time you need to flip two type constructors around, or map something then flip them around, that's most likely to be a use for `Traversable`

- Consider some undefined functions:

    ```haskell
    f :: a -> Maybe b
    f = undefined

    xs :: [a]
    xs = undefined
    ```

- If we `fmap` the function over the list, we get a `[Maybe b]`:

    ```haskell
    > :t map f xs
    map f xs :: [Maybe b]
    ```

- But what if we actually want a `Maybe [b]`?  We need to 'flip' the structures:

    ```haskell
    > :t sequenceA $ map f xs
    sequenceA $ map f xs :: Maybe [a]
    ```

- Whenever we see `sequence` or `sequenceA` used with `map` or `fmap`, just use `traverse`:

    ```haskell
    > :t traverse f xs
    traverse f xs :: Maybe [b]
    ```


## 21.6 - Morse code example

- For an example of using `Traversable`, consider the Morse code example from earlier:

    ```haskell
    import qualified Data.Map as M

    type Morse = String

    letterToMorse :: M.Map Char Morse
    -- letterToMorse is a map of chars to their morse representation

    charToMorse :: Char -> Maybe Morse
    charToMorse c = M.lookup

    > charToMorse 'f'
    Just "..-."

    > charToMorse '%'
    Nothing
    ```

- Say we want to convert a `String` to its morse representation, by `fmap`ping `charToMorse` over the `String`:

    ```haskell
    -- If all the characters are valid...
    > fmap charToMorse "hello"
    [Just "....",Just ".",Just ".-..",Just ".-..",Just "---"]

    -- If there are some characters without Morse representations...
    > fmap charToMorse "wh@t?"
    [Just ".--",Just "....",Nothing,Just "-",Nothing]
    ```

- If we want to return either `Just` the Morse representation or `Nothing`, we can use `sequence`:

    ```haskell
    > sequence $ fmap charToMorse "hello"
    Just ["....", ".", ".-..", ".-..", "---"]

    > sequence $ fmap charToMorse "wh@t?"
    Nothing
    ```

- But instead of `sequence $ fmap` we can just use `traverse`:

    ```haskell
    > traverse charToMorse "hello"
    Just ["....", ".", ".-..", ".-..", "---"]

    > traverse charToMorse "wh@t?"
    Nothing
    ```


## 21.8 - Multiple HTTP Requests

- Say we're using [wreq](http://hackage.haskell.org/package/wreq) to make HTTP calls:

    ```haskell
    import Data.ByteString.Lazy hiding (map)
    import Network.Wreq

    urls :: [String]
    urls = [ "http://httpbin.org/ip"
           , "http://httpbin.org/bytes/5"
           ]

    mappingGet :: [IO (Response ByteString)]
    mappingGet = map get urls
    ```

- Say that, instead of a list of IO actions that we can perform to get a respnose, we want one big IO action that produces a list of all responses:
    - Then we want a `IO [Response ByteString]` instead of an `[IO (Response ByteString)]`
    - This is where `Traversable` comes in, because we want to switch the `IO` and `[]`.

- Example:

    ```haskell
    traversedUrls :: IO [Response ByteString]
    traversedUrls = traverse get urls

    -- We can then call this like:
    getResponses :: IO ()
    getResponses = do
        responses <- traversedUrls
        print responses
    ```



## 21.9 - `Traversable` instances

- `Traversable` instance for `Either` looks like:

    ```haskell
    data Either a b =
          Left a
        | Right b
        deriving (Eq, Ord, Show)

    instance Traversable (Either a) where
        traverse _ (Left x)  = pure (Left x)
        traverse f (Right y) = Right <$> f y
    ```

- For a two-tuple, we have:

    ```haskell
    instance Traversable ((,) a) where
        traverse f (x, y) = (,) x <$> f y
    ```


## 21.10 - `Traversable` laws

- Like `Functor`, `Applicative` and `Monad`, `Traversable` must satisfy laws.

- _Naturality_ - function composition behaves in unsurprising ways with respect to a traversed function `f`:

    ```haskell
    t . traverse f = traverse (t . f)
    ```

- _Identity_ - traversing the data constructor of the `Identity` type over a value produces the same result as just putting the value directly into `Identity`:

    ```haskell
    traverse Identity = Identity
    ```

- _Composition_ - we can collapse sequential traversals into a single traversal, using the `Compose` datatype

    ```haskell
    traverse (Compose . fmap g . f) =
                Compose . fmap (traverse g) . traverse f
    ```


## 21.11 - QuickChecking `Traversable` instances

- The _checkers_ library can also be used to check `Traversable` laws:

    ```haskell
    type TI = []

    main = do
        let trigger = undefined :: TI (Int, Int, [Int])
        quickBatch (traversable trigger)
    ```
