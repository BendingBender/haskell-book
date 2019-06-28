module Instances_Exercises.Three where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Semigroup
import Instances_Exercises.Properties.Semigroup
import Instances_Exercises.Properties.Functor
import Instances_Exercises.Properties.Foldable

data Three a b c = Three a b c deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure c = Three mempty mempty c
    (<*>) (Three a b f) (Three a' b' c) = Three (a `mappend` a') (b `mappend` b') (f c)

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c
    foldr f initial (Three _ _ c) = f c initial

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

type ThreeS = Three String String String
type ThreeSI = Three String String Int
type ThreeAssoc = ThreeS -> ThreeS -> ThreeS -> Bool
type ThreeCompose = Fun Int Int -> Fun Int Int -> ThreeSI -> Bool
type SSI = (String, String, Int)
type ThreeFoldr = (Fun (Int, String) String) -> String -> ThreeSI -> Property

main :: IO ()
main = do
    quickCheck (prop_semigroupAssoc :: ThreeAssoc)
    quickCheck (prop_FunctorIdentity :: ThreeSI -> Bool)
    quickCheck (prop_FunctorCompose' :: ThreeCompose)
    quickCheck (prop_foldr :: ThreeFoldr)
    let appWitness :: Three (Sum Int) (Product Int) SSI
        appWitness = undefined
    quickBatch $ applicative appWitness
    let foldableWitness :: Three Int Int (String, String, String, Int, Int)
        foldableWitness = undefined
    quickBatch $ foldable foldableWitness