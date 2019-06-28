module Ch9_lists.Exercises.CaesarCipher where

import Data.Char

data Direction = Encode | Decode

caesar' :: Direction -> Int -> String -> String
caesar' direction shiftCount text =
    map (shiftCharBy $ op direction $ shiftCount) text
    where
        op Encode = (+)
        op Decode = flip (-)

shiftCharBy :: (Int -> Int) -> Char -> Char
shiftCharBy _ ' ' = ' '
shiftCharBy shiftBy char = chr . wrapAroud $ charShifted
    where
        aIdx = ord 'A'
        charShifted = shiftBy (ord char)
        wrapAroud idx = ((idx - aIdx) `mod` 26) + aIdx

caesar :: Int -> String -> String
caesar shiftCount = caesar' Encode shiftCount

unCaesar :: Int -> String -> String
unCaesar shiftCount = caesar' Decode shiftCount

main :: IO ()
main = do
    print ((unCaesar 5 . caesar 5 $ "HIWORLD") == "HIWORLD")