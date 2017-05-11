--------------------------------------
-- Chapter 20 - Foldable - Examples --
--------------------------------------
main :: IO ()
main = undefined

-- 20.4 - Foldable Instances
----------------------------

-- Identity
data Identity a =
    Identity a

instance Foldable Identity where
    foldMap f (Identity x) = f x


-- Maybe
data Optional a =
    Nada | Yep a

instance Foldable Optional where
    foldMap f Nada    = mempty
    foldMap f (Yep x) = f x
