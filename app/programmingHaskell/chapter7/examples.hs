module Chapter7 where

-------------------------
-- 7.5 - Pattern Matching
-------------------------

-- We can pattern match on numbers, using _ to match anything unmatched
-- Note that the _ match needs to go last
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False


-- We can do pattern matching against data constructors
newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
            | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistered User"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ show acctNum


-- It can also be done on sum types

data WherePenguinsLive = Galapagos
                       | Antarctic
                       | Australia
                       | SouthAfrica
                       | SouthAmerica
                       deriving (Eq, Show)

-- and product types

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

doesItLiveInSA :: Penguin -> Bool
doesItLiveInSA (Peng whereitlives) = whereitlives == SouthAfrica


-- Pattern matching on tuples

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y



-------------------------
-- 7.6 - Case Expressions
-------------------------

-- `case` expressions give an alternative way to write `if-then-else` expressions
funcZ :: (Num a, Eq a) => a -> String
funcZ x = if x + 1 == 1
             then "AWESOME"
             else "wut"

funcZ' :: (Num a, Eq a) => a -> String
funcZ' x = case x + 1 == 1 of
                True -> "AWESOME"
                False -> "wut"

pal :: Eq a => [a] -> String
pal xs = case xs == reverse xs of
              True  -> "yes"
              False -> "no"
