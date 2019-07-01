module Ch7_functional_patterns.Exercises.GrabBag where

addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \x -> x + 1

addFive :: (Num a, Ord a) => a -> a -> a
addFive = \x y -> (if x > y then y else x) + 5

mflip :: (a -> b -> c) -> b -> a -> c
mflip f x y = f y x