module Instances_Exercises.FourPrimePrime where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four'' a b = Four'' a b b b deriving (Show, Eq)

instance Foldable (Four'' a) where
    foldMap f (Four'' _ b b' b'') = f b <> f b' <> f b''
    foldr f initial (Four'' _ b b' b'') = f b . f b' . f b'' $ initial

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four'' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        b'' <- arbitrary
        return $ Four'' a b b' b''

instance (Eq a, Eq b) => EqProp (Four'' a b) where
    (=-=) = eq

main :: IO ()
main = do
    let foldableWitness :: Four'' Int (String, String, String, Int, Int)
        foldableWitness = undefined
    quickBatch $ foldable foldableWitness