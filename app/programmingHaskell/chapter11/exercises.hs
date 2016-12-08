---------------
-- Exercises --
---------------

-- Exercises: Vehicles
----------------------

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir


-- Question 2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


-- Question 3
-- This is a partial function - caling `getManu doge` throws an exception
getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x


-- Question 5
data Size = Size Integer
data Vehicle' = Car' Manufacturer Price
              | Plane' Airline Size

isCar' :: Vehicle' -> Bool
isCar' (Car' _ _) = True
isCar' _ = False

isPlane' :: Vehicle' -> Bool
isPlane' (Plane' _ _) = True
isPlane' _ = False

