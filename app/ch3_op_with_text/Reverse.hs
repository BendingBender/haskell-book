module Reverse where

rvrs :: String -> String
rvrs s = concat [
    take 7 $ drop 9 s,
    " ",
    take 2 $ drop 6 s,
    " ",
    take 5 s]

main :: IO ()
main = print $ rvrs "Curry is awesome"