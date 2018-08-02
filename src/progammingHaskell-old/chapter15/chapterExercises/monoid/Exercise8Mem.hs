module Exercise8Mem where

import Data.Semigroup
import Test.QuickCheck
import Text.Show.Functions

newtype Mem s a =
    Mem { runMem :: s -> (a, s) }
    deriving Show

instance Semigroup a => Semigroup (Mem s a) where
    (Mem f) <> (Mem g) = Mem $ \t -> ((fst (f t)) <> (fst (g t)), snd (g (snd (f t))))

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
    mempty = Mem (\s -> (mempty, s))
    mappend = (<>)

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Mem s a) where
    arbitrary = do
        f <- arbitrary
        return $ Mem f

memAssoc :: (Eq s, Eq a, Semigroup a, Monoid a) => s -> Mem s a -> Mem s a -> Mem s a -> Bool
memAssoc v a b c =     runMem (a <> (b <> c)) v
                    == runMem ((a <> b) <> c) v

memLeftIdentity :: (Eq s, Eq a, Semigroup a, Monoid a) => s -> Mem s a -> Bool
memLeftIdentity v m = runMem (mempty <> m) v == runMem m v

memRightIdentity :: (Eq s, Eq a, Semigroup a, Monoid a) => s -> Mem s a -> Bool
memRightIdentity v m = runMem (m <> mempty) v == runMem m v

type MemAssoc = Int -> Mem Int String -> Mem Int String -> Mem Int String -> Bool
type MemId = Int -> Mem Int String -> Bool

main :: IO ()
main = do
    quickCheck (memAssoc :: MemAssoc)
    quickCheck (memLeftIdentity :: MemId)
    quickCheck (memRightIdentity :: MemId)

testOutput :: IO ()
testOutput = let f' = Mem $ \s -> ("hi", s + 1)
                 g' = Mem $ \s -> ("there", s * 2)
    in do
        print $ runMem (f' <> mempty) 0
        print $ runMem (mempty <> f') 0
        print $ (runMem mempty 0 :: (String, Int))
        print $ runMem (f' <> mempty) 0 == runMem f' 0
        print $ runMem (mempty <> f') 0 == runMem f' 0
        print $ runMem (f' <> g') 0