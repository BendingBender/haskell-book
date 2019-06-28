module Ciphers where

import System.IO
import Text.Read (readMaybe)
import Ch9_lists.Exercises.CaesarCipher (caesar)
import Ch11_types.Exercises.VigenereCipher (vigenere)

calcCaesar :: IO ()
calcCaesar = do
    hSetBuffering stdout NoBuffering
    putStr "Enter shift count for cipher: "
    count <- getLine
    case readMaybe count of
        Nothing -> do putStrLn $ "'" ++ count ++ "' is not a valid number!"
        (Just c) -> do
            putStr "Enter plaintext to enrypt to caesar: "
            plain <- getLine
            putStrLn $ "Enrypted: " ++ (caesar c plain)

calcVigenere :: IO ()
calcVigenere = do
    hSetBuffering stdout NoBuffering
    putStr "Enter encryption keyword for cipher: "
    keyword <- getLine
    putStr "Enter plaintext to enrypt to vigenere: "
    plain <- getLine
    putStrLn $ "Enrypted: " ++ (vigenere keyword plain)

main :: IO ()
main = do
    putStrLn "Choose the cipher to use:\n\
    \ c -> caesar\n\
    \ v -> vigenere"
    choice <- getLine
    case choice of
        "c" -> do calcCaesar
        "v" -> do calcVigenere
        _ -> do putStrLn $ "'" ++ choice ++ "' is not a valid choice"