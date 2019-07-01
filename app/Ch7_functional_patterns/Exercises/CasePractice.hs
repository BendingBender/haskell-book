module Ch7_functional_patterns.Exercises.CasePractice where

functionC :: Ord a => a -> a -> a
functionC x y =
    case xGtY of
        True -> x
        False -> y
    where xGtY = x > y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n = 
    case nEven of
        True -> n + 2
        False -> n
    where nEven = even n

nums :: (Ord a, Num a, Num b) => a -> b
nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0