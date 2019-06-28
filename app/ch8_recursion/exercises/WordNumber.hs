module Ch8_recursion.Exercises.WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = go (mod (abs n) 10)
    where 
        go 1 = "one"
        go 2 = "two"
        go 3 = "three"
        go 4 = "four"
        go 5 = "five"
        go 6 = "six"
        go 7 = "seven"
        go 8 = "eight"
        go 9 = "nine"
        go _ = "zero"

digits :: Int -> [Int]
digits n = go (abs n) []
    where go q ds
            | q == 0 = ds
            | otherwise =
                go newQ (digit:ds)
                where
                    (newQ, digit) = divMod q 10

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map digitToWord . digits $ n