module Validation where

import Test.QuickCheck (quickCheck, Arbitrary (arbitrary), oneof)
import Data.Semigroup
import Instance_Exercises.Properties.Semigroup

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    success@(Success _) <> _ = success
    (Failure _) <> success@(Success _) = success
    (Failure f) <> (Failure f') = Failure (f <> f')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Failure a, return $ Success b]

type ValidationAssoc =
    Validation String Int -> Validation String Int -> Validation String Int -> Bool

main :: IO ()
main = do
    quickCheck (prop_semigroupAssoc :: ValidationAssoc)
    let failure :: String -> Validation String Int
        failure = Failure
        success :: Int -> Validation String Int
        success = Success
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2