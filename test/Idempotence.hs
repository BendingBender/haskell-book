module Idempotence where

import Ch11_types.Exercises.AsPatterns (capitalizeWord)
import Test.Hspec
import Test.QuickCheck
import Data.List

twice :: (a -> a) -> a -> a
twice f'' = f'' . f''

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

f :: String -> Bool
f x = (capitalizeWord x == twice capitalizeWord x)
    && (capitalizeWord x == fourTimes capitalizeWord x)

f' :: String -> Bool
f' x = (sort x == twice sort x) && (sort x == fourTimes sort x)

main :: IO ()
main = hspec $ do
    describe "idempotence" $ do
        it "capitalizeWord .. twice .. fourTimes" $ do
            property f
        it "sort .. twice .. fourTimes" $ do
            property f'