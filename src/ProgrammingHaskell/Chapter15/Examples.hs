module ProgrammingHaskell.Chapter15.Examples where

import Data.Monoid
import Test.QuickCheck
import ProgrammingHaskell.Chapter15.MonoidLaws

--------------------------------------------------
-- 15.12 - Using QuickCheck to test monoid laws --
--------------------------------------------------

-- Shortcuts
type S = String
type B = Bool


-- Example of a broken Monoid instance which shows we can't
-- return `False` as an identity
data Bull = 
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools)
                          , (1, return Twoo)]

instance Semigroup Bull where
    _ <> _ = Fools

instance Monoid Bull where
    mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

runBullTests :: IO ()
runBullTests = do
    quickCheck (monoidAssoc :: BullMappend)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)
