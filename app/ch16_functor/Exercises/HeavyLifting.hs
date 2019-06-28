module HeavyLifting where

a :: [Int]
a = fmap (+1) $ read "[1]"

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c :: Num a => a -> a
c = fmap (*2) (\x -> x - 2)

d :: (Integral a, Show a) => a -> String
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi :: IO Integer
    in fmap (*3) changed