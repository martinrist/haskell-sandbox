
-----------------------
-- Exercises : Grab Bag
-----------------------

-- Question 3a

addOneIfOdd n = case odd n of
                     True -> f n
                     False -> n
                     where f n = n + 1

addOneIfOdd' n = case odd n of
                      True -> f n
                      False -> n
                      where f = \n -> n + 1


-- Question 3b
addFive x y = (if x > y then y else x) + 5

addFive' = \x -> \y -> (if x > y then y else x) + 5


-- Question 3c
mflip f = \x -> \y -> f y x

mflip' f x y = f y x


---------------------------
-- Exercises : Variety Pack
---------------------------

-- Question 1
k :: (a, b) -> a
k (x, y) = x


-- Question 2
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))



----------------------------
-- Exercises : Case Practice
----------------------------

-- Question 1
functionC x y = if x > y then x else y

functionC' x y = case x > y of
                      True  -> x
                      False -> y


-- Question 2

ifEvenAdd2 n = if even n then n+2 else n
ifEvenAdd2' n = case even n of
                     True  -> n+2
                     False -> n

-- Question 3
nums :: (Num a, Ord a) => a -> a
nums x = case compare x 0 of
              LT -> -1
              GT -> 1
              EQ -> 0



---------------------------
-- Exercises : Artful Dodgy
---------------------------
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2



-------------------------
-- Exercises : Guard Duty
-------------------------

-- Question 1
grade :: (Fractional a, Ord a) => a -> Char
grade x
      | y > 0.9   = 'A'
      | y >= 0.8  = 'B'
      | y >= 0.7  = 'C'
      | y >= 0.59 = 'D'
      | y < 0.59  = 'F'
      where y = x / 100

-- Question 3
pal xs
    | xs == reverse xs = True
    | otherwise        = False

-- Question 5
numbers :: (Num a, Ord a, Num b) => a -> b
numbers x
        | x < 0  = -1
        | x == 0 = 0
        | x > 0  = 1


--------------------
-- Chapter Exercises
--------------------

-- Let's write code

-- Question 1
tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
    where (d1, _) = divMod x 10
          (_, d)  = divMod d1 10

hundredsDigit :: Integral a => a -> a
hundredsDigit x = d2
    where d = x `div` 100
          d2 = d `mod` 10


-- Question 2
foldBool :: a -> a -> Bool -> a
foldBool x y b =
    case b of
         True  -> x
         False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
    | b == True     = x
    | otherwise     = y


-- Question 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


-- Questions 4 & 5
roundTrip :: (Show a, Read a) => a -> a
-- roundTrip a = read (show a)
-- Pointfree version
roundTrip = read . show

main = do
    print (roundTrip 4)
    print (id 4)

-- Question 6
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show
