import           Data.ByteString.Lazy hiding (map)
import qualified Data.Map             as M
import           Network.Wreq


-----------------------------------------
-- Chapter 21 - Traversable - Examples --
-----------------------------------------
main :: IO ()
main = undefined


-- 21.1 - Motivation for Traversable

deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
deleteIfNegative x = if x < 0 then Nothing else Just x

rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]
rejectWithNegatives = traverse deleteIfNegative


-- 21.5 - Uses for Traversable

-- Some dummy, undefined functions, so we can inspect the types

f :: a -> Maybe b
f = undefined

xs :: [a]
xs = undefined



-- 21.6 - Morse code examples
type Morse = String

letterToMorse :: M.Map Char Morse
letterToMorse = M.fromList [
      ('a', ".-")
    , ('b', "-...")
    , ('c', "-.-.")
    , ('d', "-..")
    , ('e', ".")
    , ('f', "..-.")
    , ('g', "--.")
    , ('h', "....")
    , ('i', "..")
    , ('j', ".---")
    , ('k', "-.-")
    , ('l', ".-..")
    , ('m', "--")
    , ('n', "-.")
    , ('o', "---")
    , ('p', ".--.")
    , ('q', "--.-")
    , ('r', ".-.")
    , ('s', "...")
    , ('t', "-")
    , ('u', "..-")
    , ('v', "...-")
    , ('w', ".--")
    , ('x', "-..-")
    , ('y', "-.--")
    , ('z', "--..")
    , ('1', ".----")
    , ('2', "..---")
    , ('3', "...--")
    , ('4', "....-")
    , ('5', ".....")
    , ('6', "-....")
    , ('7', "--...")
    , ('8', "---..")
    , ('9', "----.")
    , ('0', "-----")
    ]

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse


-- 21.8 - Do all the things

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/user-agent"
        ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls

getResponses :: IO ()
getResponses = do
    responses <- traversedUrls
    print responses


-- 21.9 - Traversable instances

data Either' a b =
      Left' a
    | Right' b
    deriving (Eq, Ord, Show)

instance Functor (Either' a) where
    fmap _ (Left' x)  = Left' x
    fmap f (Right' y) = Right' (f y)

instance Applicative (Either' e) where
    pure           = Right'
    Left' e <*> _  = Left' e
    Right' f <*> r = fmap f r

instance Foldable (Either' a) where
    foldMap _ (Left' _)  = mempty
    foldMap f (Right' y) = f y

instance Traversable (Either' a) where
    traverse _ (Left' x)  = pure (Left' x)
    traverse f (Right' y) = Right' <$> f y


instance Traversable ((,) a) where
    traverse = undefined
