{-# LANGUAGE FlexibleInstances #-}

module QuickCheck where

import Test.Hspec
import Test.QuickCheck

import Data.List (sort)

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where
        go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

plusAssociative :: Integer -> Integer -> Integer -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Integer -> Integer -> Bool
plusCommutative x y = x + y == y + x

mulAssociative :: Integer -> Integer -> Integer -> Bool
mulAssociative x y z = x * (y * z) == (x * y) * z

mulCommutative :: Integer -> Integer -> Bool
mulCommutative x y = x * y == y * x

expAssociative :: Integer -> Integer -> Integer -> Bool
expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

expCommutative :: Integer -> Integer -> Bool
expCommutative x y = x ^ y == y ^ x

validQuotInputPairGen :: Gen (Int, Int)
validQuotInputPairGen = do
    x <- arbitrary
    y <- arbitrary
    return (x, if y == 0 then y + 1 else y)

instance Show (Int -> Int) where
    show _ = "Int -> Int"

square :: Num a => a -> a
square x = x * x

main :: IO ()
main = hspec $ do
    describe "half" $ do
        it "output multiplied by 2 should be equal to input" $ do
            property $ \x -> halfIdentity x == (x::Double)
    describe "sort" $ do
        it "list passed through sort should be sorted" $ do
            property $ \x -> listOrdered $ sort (x::[Int])
    describe "plus" $ do
        it "is associative" $ do
            property plusAssociative
        it "is commutative" $ do
            property plusCommutative
    describe "mul" $ do
        it "is associative" $ do
            property mulAssociative
        it "is commutative" $ do
            property mulCommutative
    describe "relationships quot-rem and div-mod" $ do
        it "quot-rem" $ do
            property $ forAll
                validQuotInputPairGen
                (\(x, y) -> (quot x y) * y + (rem x y) == x)
        it "div-mod" $ do
            property $ forAll
                validQuotInputPairGen
                (\(x, y) -> (div x y) * y + (mod x y) == x)
    describe "exp (^)" $ do
        it "is associative" $ do
            property expAssociative
        it "is commutative" $ do
            property expCommutative
    describe "reverse" $ do
        it "reverse list twice produces original list" $ do
            property $ \x -> (reverse . reverse $ x) == (x::[Int])
    describe "$" $ do
        it "f $ a = f a" $ do
            property $ \f a -> (f $ a) == (f::Int -> Int) (a::Int)
        it "f . g = \\x -> f (g x)" $ do
            property $ \f g a -> ((f::Int->Int) . (g::Int->Int)) a == (\a' -> f (g a')) a
    describe "foldr" $ do
        it "(:) == (++)" $ do
            property $
                \xs xs' -> foldr (\x xs'' -> x:xs'') xs' xs == xs ++ (xs'::[Int])
        it "(++) [] == concat" $ do
            property $
                \xs -> foldr (\x xs' -> x ++ xs') [] xs == concat (xs::[[Int]])
    describe "IsThatSo?" $ do
        it "n xs = length (take n xs) == n" $ do
            property $
                \n xs -> length (take n (xs::[Int])) == n
        it "x = (read (show x)) == x" $ do
            property $ \x -> (read (show x)) == (x::Int)
    describe "square" $ do
        it "squareIdentity = square . sqrt" $ do
            property $ \x -> (square . sqrt $ x) == (x::Double)