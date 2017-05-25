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
