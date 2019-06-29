module Instances_Exercises.Constant where

import Instances_Exercises.Properties.Foldable
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant f) (Constant g) = Constant $ mappend f g

instance Foldable (Constant a) where
    foldMap _ _ = mempty
    foldr _ initial _ = initial

instance Traversable (Constant a) where
    traverse _ (Constant a) = pure $ Constant a

instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = do
        a <- arbitrary
        return $ Constant a

instance (Eq a, Eq b) => EqProp (Constant a b) where
    (=-=) = eq

type ConstIS = Constant Int String
type ConstFoldr = (Fun (String, Int) Int) -> Int -> ConstIS -> Property

main :: IO ()
main = do
    let f = Constant (Sum (1::Int))
        g = Constant (Sum 2)
    print $ f <*> g
    print $ (pure 1 :: Constant String Int)
    quickCheck (prop_foldr :: ConstFoldr)
    let foldableWitness :: Constant Int (String, String, String, Int, Int)
        foldableWitness = undefined
    quickBatch $ foldable foldableWitness
    let traversableWitness :: Constant Int (Int, Int, [Int])
        traversableWitness = undefined
    quickBatch $ traversable traversableWitness
