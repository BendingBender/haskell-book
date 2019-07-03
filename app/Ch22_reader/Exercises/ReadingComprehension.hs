module Ch22_reader.Exercises.ReadingComprehension where

import Ch22_reader.Reader

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 = ((<*>) .) . (<$>)

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader


newtype HumanName =
    HumanName String
    deriving (Eq, Show)

newtype DogName =
    DogName String
    deriving (Eq, Show)

newtype Address =
    Address String
    deriving (Eq, Show)

data Person = Person
    { humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    } deriving (Eq, Show)

data Dog = Dog
    { dogsName :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = do
    name <- Reader dogName
    addy <- Reader address
    return $ Dog name addy