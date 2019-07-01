module Ch10_folds.Exercises.Fibs where

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibs20th :: [Integer]
fibs20th = take 20 (fibs)

fibsLess100 :: [Integer]
fibsLess100 = takeWhile (< 100) (fibs)

fibsN :: Int -> Integer
fibsN = (!!) fibs

fac :: [Integer]
fac = scanl (*) 1 [2..]
