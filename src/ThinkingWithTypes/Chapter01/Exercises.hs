module ThinkingWithTypes.Chapter01.Exercises where

-- Exercise 1.4-i
-- Proof that `(a^b)^c = a^(b×c)`

fromExpOfExp :: (b -> c -> a) -> (b, c) -> a
fromExpOfExp f (b, c) = f b c

toExpOfExp :: ((b, c) -> a) -> b -> c -> a
toExpOfExp f b c = f (b, c)


-- Exercise 1.4-ii
-- Proof that `a^b x a^c = a ^ (b + c)`

fromProdOfExp :: (b -> a, c -> a) -> Either b c -> a
fromProdOfExp (f, _) (Left  b) = f b
fromProdOfExp (_, g) (Right c) = g c

toProdOfExp :: (Either b c -> a) -> (b -> a, c -> a)
toProdOfExp f = (f . Left, f . Right)


-- Exercise 1.4-iii

fromExpOfProd :: (c -> (a, b)) -> (c -> a, c -> b)
fromExpOfProd f = (fst . f, snd . f)

toExpOfProd :: (c -> a, c -> b) -> c -> (a, b)
toExpOfProd (f, g) c = (f c, g c)