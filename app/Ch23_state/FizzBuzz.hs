module Ch23_state.FizzBuzz where

import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0  = "Buzz"
           | n `mod` 3 == 0  = "Fizz"
           | otherwise       = show n

fizzbuzzFromTo' :: Integer -> Integer -> [String]
fizzbuzzFromTo' from to =
    let go curr xs | curr < from = xs
                   | otherwise   = go (curr - 1) (fizzBuzz curr : xs)
    in  go to []

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to =
    execState (mapM_ addResult [to, to - 1 .. from]) []

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
    execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

main :: IO ()
main = do
    -- mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
    mapM_ putStrLn $ fizzbuzzFromTo 1 100
