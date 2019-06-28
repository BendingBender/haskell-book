module EnumFromTo where

eft :: (Enum a, Eq a) => a -> a -> [a]
eft x y = go x y []
    where go stop curr list
            | curr == stop = (curr:list)
            | otherwise = go stop (pred curr) (curr:list)

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft