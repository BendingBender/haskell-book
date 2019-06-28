module Ciphers where

import Ch9_lists.Exercises.CaesarCipher (caesar, unCaesar)
import Ch11_types.Exercises.VigenereCipher (vigenere, unVigenere)
import Test.QuickCheck

charGen :: Gen Char
charGen = elements (' ' : ['A'..'Z'])

stringGen :: Gen String
stringGen = listOf charGen

nonEmptyStringGen :: Gen String
nonEmptyStringGen = listOf1 charGen

intGen :: Gen Int
intGen = do
    i <- arbitrary
    return (abs i `mod` 26)

intStrGen :: Gen (Int, String)
intStrGen = do
    i <- intGen
    s <- stringGen
    return (i, s)

strStrGen :: Gen (String, String)
strStrGen = do
    s <- nonEmptyStringGen
    s' <- stringGen
    return (s, s')

prop_caesarEncDecEqSame :: Property
prop_caesarEncDecEqSame =
    forAll intStrGen
        (\(i, str) -> (unCaesar i $ caesar i str) == str)

prop_vigenereEncDecEqSame :: Property
prop_vigenereEncDecEqSame =
    forAll strStrGen
        (\(key, str) -> (unVigenere key $ vigenere key str) == str)

main :: IO ()
main = do
    quickCheck prop_caesarEncDecEqSame
    quickCheck prop_vigenereEncDecEqSame