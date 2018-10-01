module ProgrammingHaskell.Chapter07.Examples where

-------------------------
-- 7.4 - Pattern Matching
-------------------------
-- We can pattern match on numbers, using _ to match anything unmatched
-- Note that the _ match needs to go last
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

-- We can do pattern matching against data constructors
newtype Username =
    Username String

newtype AccountNumber =
    AccountNumber Integer

data User
    = UnregisteredUser
    | RegisteredUser Username
                     AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistered User"
printUser (RegisteredUser (Username name) (AccountNumber acctNum)) =
    putStrLn $ name ++ " " ++ show acctNum

-- It can also be done on sum types
data WherePenguinsLive
    = Galapagos
    | Antarctic
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

-- and product types
data Penguin =
    Peng WherePenguinsLive
    deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

doesItLiveInSA :: Penguin -> Bool
doesItLiveInSA (Peng whereitlives) = whereitlives == SouthAfrica

-- Pattern matching on tuples
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

-------------------------
-- 7.5 - Case Expressions
-------------------------
data DayOfWeek
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun
    deriving (Eq, Show, Enum)

nextDay :: DayOfWeek -> DayOfWeek
nextDay d =
    case d == Sun of
        True -> Mon
        False -> succ d

-- `case` expressions give an alternative way to write `if-then-else` expressions
funcZ :: (Num a, Eq a) => a -> String
funcZ x =
    if x + 1 == 1
        then "AWESOME"
        else "wut"

funcZ' :: (Num a, Eq a) => a -> String
funcZ' x =
    case x + 1 == 1 of
        True -> "AWESOME"
        False -> "wut"

pal :: Eq a => [a] -> String
pal xs =
    case xs == reverse xs of
        True -> "yes"
        False -> "no"

-------------------------------
-- 7.6 - Higher-order Functions
-------------------------------
data Employee
    = Coder
    | Manager
    | Veep
    | CEO
    deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
    case compare e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither employee is the boss"
        LT -> (flip reportBoss) e e'

-- This version allows us to supply a 'comparator':
employeeRank' ::
       (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank' f e e' =
    case f e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither employee is the boss"
        LT -> (flip reportBoss) e e'

---------------
-- 7.7 - Guards
---------------
myAbs :: Integer -> Integer
myAbs x
    | x < 0 = (-x)
    | otherwise = x
