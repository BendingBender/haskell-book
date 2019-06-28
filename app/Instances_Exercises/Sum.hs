module Instances_Exercises.Sum where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Instances_Exercises.Properties.Functor

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
    pure = Second
    (<*>) (First a) _ = First a
    (<*>) _ (First a) = First a
    (<*>) (Second f) b = f <$> b

instance Monad (Sum a) where
    return = pure
    (>>=) (First a) _ = First a
    (>>=) (Second b) f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ First a, return $ Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

data Sum' b a = First' a | Second' b deriving (Eq, Show)

instance Functor (Sum' b) where
    fmap f (First' a) = First' $ f a
    fmap _ (Second' b) = Second' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum' b a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ First' a, return $ Second' b]

type SumSI = Sum String Int
type SumCompose = Fun Int Int -> Fun Int Int -> SumSI -> Bool
type Sum'SI = Sum' String Int
type Sum'Compose = Fun Int Int -> Fun Int Int -> Sum'SI -> Bool
type SSI = (String, String, Int)

main :: IO ()
main = do
    quickCheck (prop_FunctorIdentity :: SumSI -> Bool)
    quickCheck (prop_FunctorCompose' :: SumCompose)
    let s :: Sum String SSI
        s = undefined
    quickBatch $ applicative s
    quickBatch $ monad s
    quickCheck (prop_FunctorIdentity :: Sum'SI -> Bool)
    quickCheck (prop_FunctorCompose' :: Sum'Compose)