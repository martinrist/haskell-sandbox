module ProgrammingHaskell.Chapter17.Examples where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


---------------------------
-- Chapter 17 - Examples --
---------------------------

-- 17.5 - Applicative in use

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)


-- Maybe applicative example to validate Persons

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if length s > maxLen
       then Nothing
       else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

-- Smart constructors for Name and Address include validation

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = Address <$> validateLength 100 s

-- How do we create a smart constructor for Person?

data Person =
    Person Name Address
    deriving (Eq, Show)

-- First attempt
mkPerson :: String -> String -> Maybe Person
mkPerson n a =
    case mkName n of
         Nothing -> Nothing
         Just n' ->
             case mkAddress a of
                  Nothing -> Nothing
                  Just a' -> Just $ Person n' a'

-- Second attempt
mkPerson' :: String -> String ->  Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a


-- Example with Cows

data Cow = Cow {
      name   :: String
    , age    :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

mkCow :: String -> Int -> Int -> Maybe Cow
mkCow name' age' weight' = Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'

-- alternative using `liftA3`

mkCow' :: String -> Int -> Int -> Maybe Cow
mkCow' name' age' weight' = liftA3 Cow (noEmpty name')
                                       (noNegative age')
                                       (noNegative weight')



-----------------------------------------
-- 17.7 - QuickCheck for Applicative laws
-----------------------------------------

data Bull =
      Fools
    | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary =
        frequency [ (1, return Fools)
                  , (1, return Twoo) ]

instance Semigroup Bull where
    _ <> _ = Fools

instance Monoid Bull where
    mempty = Fools

instance EqProp Bull where (=-=) = eq

main :: IO ()
main = quickBatch (monoid Twoo)


-- 17.9 - ZipList Monoid

instance Semigroup a => Semigroup (ZipList a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList a) where
    mempty = ZipList []

-- `ZipList` and `Sum` already have these instances
-- instance Arbitrary a => Arbitrary (ZipList a) where
--     arbitrary = ZipList <$> arbitrary

-- instance Arbitrary a => Arbitrary (Sum a) where
--     arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq