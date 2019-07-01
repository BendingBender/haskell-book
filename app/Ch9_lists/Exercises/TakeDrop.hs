module Ch9_lists.Exercises.TakeDrop where

import Ch9_lists.Exercises.Split (split)

myWords :: String -> [String]
myWords = split ' '

main :: IO ()
main = print $ myWords "sheryl wants fun"
