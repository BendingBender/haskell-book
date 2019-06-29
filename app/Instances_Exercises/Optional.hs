module Instances_Exercises.Optional where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        oneof [return Nada, return $ Yep a]

instance (Eq a) => EqProp (Optional a) where
    (=-=) = eq

main :: IO ()
main = do
    let functorWitness :: Optional (String, String, Int)
        functorWitness = undefined
    quickBatch $ functor functorWitness
    let foldableWitness :: Optional (String, String, String, Int, Int)
        foldableWitness = undefined
    quickBatch $ foldable foldableWitness
    let traversableWitness :: Optional (Int, Int, [Int])
        traversableWitness = undefined
    quickBatch $ traversable traversableWitness