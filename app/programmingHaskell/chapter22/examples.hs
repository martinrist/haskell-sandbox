import           Control.Applicative (liftA2)

-----------------------------------------
-- Chapter 22 - Reader - Examples --
-----------------------------------------
main :: IO ()
main = undefined


-- 22.2 - Examples
------------------

-- `boop` and `doop` are just two simple Integer -> Integer functions
boop = (*2)
doop = (+10)

-- We can compose them in the normal manner
bip :: Integer -> Integer
bip = boop . doop

-- We can also compose using `fmap`
bloop :: Integer -> Integer
bloop = fmap boop doop

-- Extend to using Applicatives
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

-- .. or equivalently
duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- The same, but in Monadic context
boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)
