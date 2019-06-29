module Instances_Exercises.ThreePrime where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Instances_Exercises.Properties.Functor
import Instances_Exercises.Properties.Foldable

data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
    pure b = Three' mempty b b
    (<*>) (Three' a f f') (Three' a' b b') = Three' (a `mappend` a') (f b) (f' b')

instance Foldable (Three' a) where
    foldMap f (Three' _ b b') = f b `mappend` f b'
    foldr f initial (Three' _ b b') = f b . f b' $ initial

instance Traversable (Three' a) where
    traverse f (Three' a b b') = Three' a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

type Three'SI = Three' String Int
type Three'Compose = Fun Int Int -> Fun Int Int -> Three'SI -> Bool
type SSI = (String, String, Int)
type Three'Foldr = (Fun (Int, String) String) -> String -> Three'SI -> Property

main :: IO ()
main = do
    quickCheck (prop_FunctorIdentity :: Three'SI -> Bool)
    quickCheck (prop_FunctorCompose' :: Three'Compose)
    quickCheck (prop_foldr :: Three'Foldr)
    let appWitness :: Three' (Sum Int) SSI
        appWitness = undefined
    quickBatch $ applicative appWitness
    let foldableWitness :: Three' Int (String, String, String, Int, Int)
        foldableWitness = undefined
    quickBatch $ foldable foldableWitness
    let traversableWitness :: Three' Int (Int, Int, [Int])
        traversableWitness = undefined
    quickBatch $ traversable traversableWitness