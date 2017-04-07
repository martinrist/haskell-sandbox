import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad ((>=>))

---------------------------
-- Chapter 18 - Examples --
---------------------------

-- 18.3 - do Syntax and Monads

-- Basic `do` syntax
sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

-- Desugared using `>>` - same output as above
sequencing' :: IO ()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

-- Desugared using '*>' - same output as above
sequencing'' :: IO ()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"


-- This time we use the variable-binding version of `do`
binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

-- This is the desugared form, using >>=
binding' :: IO ()
binding' =
    getLine >>= putStrLn


-- Example using both sequencing and binding
bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn ("Hello " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "Please enter your name:" >>
    getLine >>=
    \name -> putStrLn ("Hello " ++ name)



-- More binds => more nesting
twoBinds :: IO ()
twoBinds = do
    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn "Please enter your age:"
    age <- getLine
    putStrLn ("Hello " ++ name ++ "! You are " ++ age ++ " years old")

twoBinds' :: IO ()
twoBinds' =
    putStrLn "Please enter your name:" >>
    getLine >>=
    \name ->
    putStrLn "Please enter your age:" >>
    getLine >>=
    \age ->
    putStrLn ("Hello " ++ name ++ "! You are " ++ age ++ " years old")


-- 18.4 - Monad Examples in Use
-------------------------------

-- Maybe

data Cow = Cow {
     name   :: String
   , age    :: Int
   , weight :: Int
  } deriving (Eq, Show)

-- Validation functions
noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s  = Just s

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
          then Nothing
          else Just c

-- This is the nasty way of creating a smart constructor for a Cow
mkCow :: String -> Int -> Int -> Maybe Cow
mkCow name age weight =
    case noEmpty name of
         Nothing -> Nothing
         Just namey ->
             case noNegative age of
                  Nothing -> Nothing
                  Just agey ->
                      case noNegative weight of
                           Nothing -> Nothing
                           Just weighty ->
                               weightCheck (Cow namey agey weighty)

-- A much nicer way of doing it
mkCow' :: String -> Int -> Int -> Maybe Cow
mkCow' name age weight = do
    namey   <- noEmpty name
    agey    <- noNegative age
    weighty <- noNegative weight
    weightCheck (Cow namey agey weighty)


f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n


g :: Integer -> Maybe Integer
g i =
    if even i
    then Just (i + 1)
    else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
    a <- f n
    b <- g a
    c <- h b
    pure (a, b, c)


-- Either

-- years ago
type Founded = Int

-- number of coders
type Coders = Int

data SoftwareShop =
    Shop {
        founded     :: Founded
      , programmers :: Coders
    } deriving (Eq, Show)

-- possible error conditions
data FoundedError =
      NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0     = Left $ NegativeYears n
    | n > 500   = Left $ TooManyYears n
    | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0     = Left $ NegativeCoders n
    | n > 5000  = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded     <- validateFounded years
    programmers <- validateCoders coders
    if programmers > div founded 10
       then Left $ TooManyCodersForYears founded programmers
       else Right $ Shop founded programmers


-- 18.5 - Monad Laws

-- A datatype like Identity, but with an integer that gets
-- incremented on each fmap or bind - this is broken

data CountMe a =
    CountMe Integer a
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where (=-=) = eq

instance Functor CountMe where
    -- This is broken, because we can't change the structure
    --fmap f (CountMe i a) = CountMe (i + 1) (f a)
    -- We need to leave the structure unchanged
    fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe 0
    (CountMe i f) <*> (CountMe i' a) = CountMe (i + i') (f a)

instance Monad CountMe where
    return = pure
    CountMe n a >>= f =
        let CountMe n' b = f a
            in CountMe (n + n') b


testCountMe :: IO ()
testCountMe = do
    let trigger = undefined :: CountMe (Int, String, Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger


-- 18.6 - Composition of monadic functions

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)

-- This won't work, because of the types...
-- mcomp f g a = f (g a)

-- ... but we can fmap f over g a then join to remove the extra structure...
-- mcomp f g a = join (f <$> (g a))

-- ... but this is just >>=:
mcomp f g a = g a >>= f


-- Outputs a prompt, then gets input
sayHi :: String -> IO String
sayHi prompt = do
    putStrLn prompt
    getLine

-- Takes a readable String and returns it wrapped in IO
readM :: Read a => String -> IO a
readM = return.read

-- Prompts for, then reads age
-- Uses Kleisli composition to stitch together sayHi and readM
getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad ((>=>))

---------------------------
-- Chapter 18 - Examples --
---------------------------

-- 18.3 - do Syntax and Monads

-- Basic `do` syntax
sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

-- Desugared using `>>` - same output as above
sequencing' :: IO ()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

-- Desugared using '*>' - same output as above
sequencing'' :: IO ()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"


-- This time we use the variable-binding version of `do`
binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

-- This is the desugared form, using >>=
binding' :: IO ()
binding' =
    getLine >>= putStrLn


-- Example using both sequencing and binding
bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn ("Hello " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "Please enter your name:" >>
    getLine >>=
    \name -> putStrLn ("Hello " ++ name)



-- More binds => more nesting
twoBinds :: IO ()
twoBinds = do
    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn "Please enter your age:"
    age <- getLine
    putStrLn ("Hello " ++ name ++ "! You are " ++ age ++ " years old")

twoBinds' :: IO ()
twoBinds' =
    putStrLn "Please enter your name:" >>
    getLine >>=
    \name ->
    putStrLn "Please enter your age:" >>
    getLine >>=
    \age ->
    putStrLn ("Hello " ++ name ++ "! You are " ++ age ++ " years old")


-- 18.4 - Monad Examples in Use
-------------------------------

-- Maybe

data Cow = Cow {
     name   :: String
   , age    :: Int
   , weight :: Int
  } deriving (Eq, Show)

-- Validation functions
noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s  = Just s

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
          then Nothing
          else Just c

-- This is the nasty way of creating a smart constructor for a Cow
mkCow :: String -> Int -> Int -> Maybe Cow
mkCow name age weight =
    case noEmpty name of
         Nothing -> Nothing
         Just namey ->
             case noNegative age of
                  Nothing -> Nothing
                  Just agey ->
                      case noNegative weight of
                           Nothing -> Nothing
                           Just weighty ->
                               weightCheck (Cow namey agey weighty)

-- A much nicer way of doing it
mkCow' :: String -> Int -> Int -> Maybe Cow
mkCow' name age weight = do
    namey   <- noEmpty name
    agey    <- noNegative age
    weighty <- noNegative weight
    weightCheck (Cow namey agey weighty)


f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n


g :: Integer -> Maybe Integer
g i =
    if even i
    then Just (i + 1)
    else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
    a <- f n
    b <- g a
    c <- h b
    pure (a, b, c)


-- Either

-- years ago
type Founded = Int

-- number of coders
type Coders = Int

data SoftwareShop =
    Shop {
        founded     :: Founded
      , programmers :: Coders
    } deriving (Eq, Show)

-- possible error conditions
data FoundedError =
      NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0     = Left $ NegativeYears n
    | n > 500   = Left $ TooManyYears n
    | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0     = Left $ NegativeCoders n
    | n > 5000  = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded     <- validateFounded years
    programmers <- validateCoders coders
    if programmers > div founded 10
       then Left $ TooManyCodersForYears founded programmers
       else Right $ Shop founded programmers


-- 18.5 - Monad Laws

-- A datatype like Identity, but with an integer that gets
-- incremented on each fmap or bind - this is broken

data CountMe a =
    CountMe Integer a
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where (=-=) = eq

instance Functor CountMe where
    -- This is broken, because we can't change the structure
    --fmap f (CountMe i a) = CountMe (i + 1) (f a)
    -- We need to leave the structure unchanged
    fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe 0
    (CountMe i f) <*> (CountMe i' a) = CountMe (i + i') (f a)

instance Monad CountMe where
    return = pure
    CountMe n a >>= f =
        let CountMe n' b = f a
            in CountMe (n + n') b


testCountMe :: IO ()
testCountMe = do
    let trigger = undefined :: CountMe (Int, String, Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger


-- 18.6 - Composition of monadic functions

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)

-- This won't work, because of the types...
-- mcomp f g a = f (g a)

-- ... but we can fmap f over g a then join to remove the extra structure...
-- mcomp f g a = join (f <$> (g a))

-- ... but this is just >>=:
mcomp f g a = g a >>= f


-- Outputs a prompt, then gets input
sayHi :: String -> IO String
sayHi prompt = do
    putStrLn prompt
    getLine

-- Takes a readable String and returns it wrapped in IO
readM :: Read a => String -> IO a
readM = return.read

-- Prompts for, then reads age
-- Uses Kleisli composition to stitch together sayHi and readM
getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"
