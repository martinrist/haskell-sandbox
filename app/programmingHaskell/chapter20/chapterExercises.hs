import Data.Monoid
import Data.Foldable

------------------------------------
-- Chapter 20 - Chapter Exercises --
------------------------------------

main :: IO ()
main = undefined

-- 1 - Constant
data Constant a b =
    Constant a

instance Foldable (Constant a) where
    foldMap _ (Constant a) = mempty


-- 2 - Two
data Two a b =
    Two a b

instance Foldable (Two a) where
    foldMap f (Two a b) = f b


-- 3 - Three
data Three a b c =
    Three a b c

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c


-- 4 - Three '
data Three' a b =
    Three' a b b

instance Foldable (Three' a) where
    foldMap f (Three' a b b') = f b'


-- 5 - Four'
data Four' a b =
    Four' a b b b

instance Foldable (Four' a) where
    foldMap f (Four' a b b' b'') = f b''



-- Filter function for foldable types
filterF :: (Applicative f, Foldable t, Monoid (f a))
            => (a -> Bool) -> t a -> f a
filterF pred = foldr (acc pred) mempty
    where acc pred a fa =
              if pred a then mappend (pure a) fa else fa
