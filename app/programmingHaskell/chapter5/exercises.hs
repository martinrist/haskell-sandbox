-- Given a type, write the function

myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, (yToZ(xToY x)))


i :: a -> a
i = id

c :: a -> b -> a
c x y = x

c' :: a -> b -> b
c' x y  = y

r :: [a] -> [a]
r = reverse

co :: (b -> c) -> (a -> b) -> a -> c
co f g x = f $ g $ x

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' f x = f x