module Palindrome where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (isPunctuation, isSpace, toLower)

transformCharsBy :: (Char -> Maybe Char) -> String -> String
transformCharsBy f str = foldr addBy "" str
    where
        addBy c cleanStr = case f c of
            Just c' -> (c':cleanStr)
            Nothing -> cleanStr

keepCharBy :: (Char -> Bool) -> Char -> Maybe Char
keepCharBy f c = if f c then Just c else Nothing

preprocessPalindrome :: String -> String
preprocessPalindrome =
    transformCharsBy (keepCharBy $ not . isPunctuation)
    . transformCharsBy (keepCharBy $ not . isSpace)
    . transformCharsBy (Just . toLower)

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    let preprocessed = preprocessPalindrome line1
    case (preprocessed == reverse preprocessed) of
        True -> putStrLn "It's a palindrome!"
        False -> do
            putStrLn "Nope!"
            exitSuccess

main :: IO ()
main = do
    palindrome