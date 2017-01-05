-----------------------
-- Chapter Exercises --
-----------------------
import Data.Maybe (fromMaybe)

-- String processing
--------------------

-- Question 1

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

-- Recursive implementation - looks a little clunky
replaceThe :: String -> String
replaceThe = unwords . replaceThe' . words
    where replaceThe' :: [String] -> [String]
          replaceThe' (w:[]) = case (notThe w) of
                                  Nothing -> "a" : []
                                  Just s  -> s : []
          replaceThe' (w:ws) = case (notThe w) of
                                  Nothing -> "a" : replaceThe' ws
                                  Just s  -> s : replaceThe' ws


-- Much more elegant version using `fromMaybe`
replaceThe' :: String -> String
replaceThe' = unwords . map ( fromMaybe "a" . notThe) . words


-- Question 2
isVowel :: Char -> Bool
isVowel = (flip elem) "aeiou"

startsWithVowel :: String -> Bool
startsWithVowel "" = False
startsWithVowel (x:xs) = isVowel x

wordPairs :: [String] -> [(String, String)]
wordPairs ws = zip ws (tail ws)

countTheBeforeVowel :: String -> Int
countTheBeforeVowel = 
          length . filter f . wordPairs . words
            where f :: (String, String) -> Bool
                  f (w1, w2) = w1 == "the" && startsWithVowel w2


-- Question 3

countVowels :: String -> Int
countVowels =  foldr ((+).length) 0 . map (\w -> (filter isVowel w)) . words


-- Validate the word
--------------------

newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = let numVowels = length (filter isVowel s)
               numConsonants = length s - numVowels in
        if (countVowels s) > (length s - countVowels s) then Nothing else Just (Word' s)


-- It's only Natural
--------------------

data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i < 0     = Nothing
    | i == 0    = Just Zero
    | otherwise = Just (Succ (fromMaybe Zero (integerToNat (i - 1))))



-- Small library for Maybe
--------------------------

-- Question 1 - Simple boolean checks
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- Question 2 - catamorphism for Maybe
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _ Nothing  = z
mayybee _ f (Just a) = f a

-- Question 3 - default fallback value
fromMaybe' :: a -> Maybe a -> a
fromMaybe' d Nothing  = d
fromMaybe' _ (Just a) = a

-- Question 4 - converting between `List` and `Maybe`
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- Question 5 - filter Nothing from list
catMaybes :: [Maybe a] -> [a]
catMaybes []            = []
catMaybes (Nothing:ms)  = catMaybes ms
catMaybes ((Just m):ms) = m : catMaybes ms

-- Question 6 - sequence
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []            = Just []
flipMaybe (Nothing:_)   = Nothing
flipMaybe ((Just m):ms) = case flipMaybe ms of
                               Nothing -> Nothing
                               Just ns -> Just (m : ns)


-- Small library for Either
---------------------------

-- Question 1
lefts' :: [Either a b] -> [a]
lefts' = foldr accumulator []
            where accumulator :: (Either a b) -> [a] -> [a]
                  accumulator (Left a) acc  = a : acc
                  accumulator _ acc = acc

-- Question 2
rights' :: [Either a b] -> [b]
rights' = foldr accumulator []
            where accumulator :: (Either a b) -> [b] -> [b]
                  accumulator (Right b) acc  = b : acc
                  accumulator _ acc = acc

-- Question 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

-- Question 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right b) = Just (f b)

-- Question 5 - general catamorphism for Either values
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

-- Question 6 - question 4 rewritten using either'
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' g = either' (\a -> Nothing) (\b -> Just (g b))


-- Unfolds
----------

-- Question 1 - myIterate using direct recursion
myIterate :: (a -> a) -> a -> [a]
myIterate f z = z : myIterate f (f z)

-- Question 2- custom myUnfoldr using direct recusion
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                     Nothing     -> []
                     Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just(b, f b))


-- Binary tree
--------------

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)


-- Question 1 - unfold for Binary Tree

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
                Nothing          -> Leaf
                Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)

-- Question 2 - Tree builder
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\b -> if b == n then Nothing else Just (b+1, b, b+1)) 0

