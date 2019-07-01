module Ch11_types.Exercises.VigenereCipher where

import Data.Char
import Lib
import Ch9_lists.Exercises.CaesarCipher (Direction (Encode, Decode), shiftCharBy)

vigenere :: String -> String -> String
vigenere = vigenere' Encode

unVigenere :: String -> String -> String
unVigenere = vigenere' Decode

vigenere' :: Direction -> String -> String -> String
vigenere' direction key msg = go 0 msg
    where
        go _ "" = ""
        go n (' ':ms) = (' ': go n ms)
        go n (m:ms) = transform n m : go (n + 1) ms
        transform x = shiftCharBy $ op direction $ shiftTimes key x
        op Encode = (+)
        op Decode = flip (-)

shiftTimes :: String -> Int -> Int
shiftTimes key idx =
    charToOffset (key !! (idx `mod` length key))
    where
        charToOffset c = ord c - aIdx
        aIdx = ord 'A'

main :: IO ()
main = do
    test
        "vigenere \"ALLY\" \"MEET AT DAWN\""
        (vigenere "ALLY" "MEET AT DAWN")
        "MPPR AE OYWY"
    test
        "unVigenere \"ALLY\" \"MPPR AE OYWY\""
        (unVigenere "ALLY" "MPPR AE OYWY")
        "MEET AT DAWN"

