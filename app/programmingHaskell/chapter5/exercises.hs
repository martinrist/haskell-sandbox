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

------------------
-- Type-Kwon-Do --
------------------

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f



data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q


data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)


munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xy ywz x = fst $ ywz $ xy $ x