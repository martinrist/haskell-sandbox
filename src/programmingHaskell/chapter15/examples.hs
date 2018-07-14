import Data.Monoid
import Test.QuickCheck

--------------------------------------------------
-- 15.12 - Using QuickCheck to test monoid laws --
--------------------------------------------------

-- Shortcuts
type S = String
type B = Bool

-- Associativity
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Identity
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


-- Example of a broken Monoid instance which shows we can't
-- return `False` as an identity
data Bull = 
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools)
                          , (1, return Twoo)]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

runBullTests :: IO ()
runBullTests = do
    quickCheck (monoidAssoc :: BullMappend)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)
