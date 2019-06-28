module Instances_Exercises.Properties.Functor where

import Test.QuickCheck.Function

prop_FunctorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
prop_FunctorIdentity f = fmap id f == f

prop_FunctorCompose :: (Eq (f c), Functor f) =>
       (a -> b)
    -> (b -> c)
    -> f a
    -> Bool
prop_FunctorCompose f g x = (fmap g . fmap f $ x) == (fmap (g . f) x)

prop_FunctorCompose' :: (Eq (f c), Functor f) =>
       Fun a b
    -> Fun b c
    -> f a
    -> Bool
prop_FunctorCompose' (Fun _ f) (Fun _ g) x = (fmap (g . f) x) == (fmap g . fmap f $ x)