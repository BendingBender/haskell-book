module Instances_Exercises.List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
    (<>) = append

instance Monoid (List a) where
    mempty = Nil
    mappend = append

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) fs as = flatMap (<$> as) fs

instance Monad List where
    return = pure
    (>>=) as f = concat' (f <$> as)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = toList <$> arbitrary

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' = take' 3000 xs
            ys' = take' 3000 ys

toList :: [a] -> List a
toList [] = Nil
toList (a:as) = Cons a $ toList as

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons a as) = Cons a $ take' (n - 1) as

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith' f as bs)

repeat' :: a -> List a
repeat' a = Cons a $ repeat' a

type SSI = (String, String, Int)

main :: IO ()
main = do
    let l :: List SSI
        l = undefined
    quickBatch $ applicative l
    quickBatch $ monad l
