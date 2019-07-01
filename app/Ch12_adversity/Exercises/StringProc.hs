{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ch12_adversity.Exercises.StringProc where

import Lib

notThe :: String -> Maybe String
notThe str
    | str == "the" = Nothing
    | otherwise = Just str

replaceThe :: String -> String
replaceThe str' = go "" (map notThe . words $ str')
    where
        go _ (str:[]) = theToA str
        go res (str:strs) = theToA str ++ " " ++ go res strs
        theToA Nothing = "a"
        theToA (Just w) = w

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel str = go (words str)
    where
        go (_:[]) = 0
        go (str':(str'':rest)) =
            case (str' == "the" && startsWithVowel str'') of
                True -> 1 + go (str'':rest)
                False -> go (str'':rest)
        startsWithVowel "" = False
        startsWithVowel (c:_) = isVovel c

isVovel :: Char -> Bool
isVovel c = any (\c' -> c' == c) "aeiou"

toVovels :: String -> String
toVovels "" = ""
toVovels (c:str) = if isVovel c
    then (c:toVovels str)
    else toVovels str

countVowels :: String -> Integer
countVowels str = go . toVovels $ str
    where
        go "" = 0
        go (_:str') = 1 + go str'


main :: IO ()
main = do
    test "notThe \"the\"" (notThe "the") Nothing
    test "notThe \"blahtheblah\"" (notThe "blahtheblah") (Just "blahtheblah")
    test "notThe \"woot\"" (notThe "woot") (Just "woot")
    test
        "replaceThe \"the cow loves us\""
        (replaceThe "the cow loves us") 
        "a cow loves us"
    test
        "countTheBeforeVowel \"the cow\""
        (countTheBeforeVowel "the cow") 
        0
    test
        "countTheBeforeVowel \"the evil cow\""
        (countTheBeforeVowel "the evil cow") 
        1
    test
        "countVowels \"the cow\""
        (countVowels "the cow") 
        2
    test
        "countVowels \"Mikolajczak\""
        (countVowels "Mikolajczak") 
        4