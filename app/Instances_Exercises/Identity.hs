module Instances_Exercises.Identity where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity a') = Identity $ a <> a'

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity $ mempty
    mappend = (<>)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> a = f <$> a

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

type SSI = (String, String, Int)

main :: IO ()
main = do
    let i :: Identity SSI
        i = undefined
    quickBatch $ monoid (undefined :: Identity String)
    quickBatch $ functor i
    quickBatch $ applicative i
    quickBatch $ monad i