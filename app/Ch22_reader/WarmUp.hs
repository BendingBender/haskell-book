module Ch22_reader.WarmUp where

import Data.Char
import Control.Applicative

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) rev cap

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    r <- rev
    c <- cap
    return (r, c)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' =
    rev >>= (\r -> cap >>= (\c -> return (r, c)))