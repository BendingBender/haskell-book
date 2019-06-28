module Exercises3 where

-- ch 3.8
emphasis :: String -> String
emphasis s = s ++ "!"

fifthLetter :: String -> String
fifthLetter s = take 1 $ drop 4 s

tenthOnwards :: String -> String
tenthOnwards s = drop 9 s

thirdChar :: String -> Char
thirdChar = (!! 2)

nthCharOfCurry :: Int -> Char
nthCharOfCurry = ("Curry is awesome" !!)