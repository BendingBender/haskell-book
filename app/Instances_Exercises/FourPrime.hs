module Instances_Exercises.FourPrime where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Instances_Exercises.Properties.Functor

data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' $ f b

instance (Monoid a) => Applicative (Four' a) where
    pure b = Four' mempty mempty mempty b
    (<*>) (Four' a a' a'' f) (Four' aa aa' aa'' b) =
        Four' (a `mappend` aa) (a' `mappend` aa') (a'' `mappend` aa'') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        a'' <- arbitrary
        b <- arbitrary
        return $ Four' a a' a'' b

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

type Four'SI = Four' String Int
type Four'Compose = Fun Int Int -> Fun Int Int -> Four'SI -> Bool
type SSI = (String, String, Int)

main :: IO ()
main = do
    quickCheck (prop_FunctorIdentity :: Four'SI -> Bool)
    quickCheck (prop_FunctorCompose' :: Four'Compose)
    let f :: Four' (Sum Int) SSI
        f = undefined
    quickBatch $ applicative f