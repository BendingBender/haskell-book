module Ch11_types.Exercises.Vehicles where

import Lib

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline =
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)
data Vehicle =
    Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir (Size 10000)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car x _) = Just x
getManu _ = Nothing

main :: IO ()
main = do
    printTest "isCar myCar" (isCar myCar)
    printTest "isCar doge" (isCar doge)
    printTest "isPlane myCar" (isPlane myCar)
    printTest "isPlane doge" (isPlane doge)
    printTest "areCars [clownCar, doge]" (areCars [clownCar, doge])
    printTest "getManu myCar" (getManu myCar)
    printTest "getManu doge" (getManu doge)