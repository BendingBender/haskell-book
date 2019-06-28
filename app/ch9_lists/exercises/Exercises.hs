module Exercises9 where

import Data.Char

onlyUpper :: String -> String
onlyUpper str = filter isUpper str

firstUpper :: String -> String
firstUpper [] = []
firstUpper (first:str) = (toUpper first):str

allUpper :: String -> String
allUpper [] = []
allUpper (first:str) = (toUpper first):allUpper str

firstCapital :: String -> Maybe Char
firstCapital [] = Nothing
firstCapital (c:_) = Just (toUpper c)

firstCapitalComposed :: String -> Maybe Char
firstCapitalComposed [] = Nothing
firstCapitalComposed str = Just . toUpper . head $ str

firstCapitalPF :: String -> Char
firstCapitalPF = toUpper . head

main :: IO ()
main = do
    print (onlyUpper "HbEfLrLxO")
    print (firstUpper "julie")
    print (allUpper "woot")
    print (firstCapital "woot")
    print (firstCapitalComposed "woot")
    print (firstCapitalPF "woot")