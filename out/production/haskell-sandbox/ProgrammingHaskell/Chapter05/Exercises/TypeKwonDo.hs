module ProgrammingHaskell.Chapter05.Exercises.TypeKwonDo where

-- Exercise 1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

-- Exercise 2
data A

data B

data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

-- Exercise 3
data X

data Y

data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- Exercise 4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f' g' = fst . g' . f'