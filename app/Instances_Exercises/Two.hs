module Instances_Exercises.Two where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Semigroup
import Instances_Exercises.Properties.Semigroup
import Instances_Exercises.Properties.Monoid
import Instances_Exercises.Properties.Functor
import Instances_Exercises.Properties.Foldable

data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance Functor (Two a) where
    fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
    pure b = Two mempty b
    (<*>) (Two a f) (Two a' b) = Two (a `mappend` a') (f b)

instance Foldable (Two a) where
    foldMap f (Two _ b) = f b
    foldr f initial (Two _ b) = f b initial

instance Traversable (Two a) where
    traverse f (Two a b) = Two a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

type TwoS = Two String String
type TwoSI = Two String Int
type TwoAssoc = TwoS -> TwoS -> TwoS -> Bool
type TwoCompose = Fun Int Int -> Fun Int Int -> TwoSI -> Bool
type SSI = (String, String, Int)
type TwoFoldr = (Fun (Int, String) String) -> String -> TwoSI -> Property

main :: IO ()
main = do
    quickCheck (prop_semigroupAssoc :: TwoAssoc)
    quickCheck (prop_monoidLeftIdentity :: TwoS -> Bool)
    quickCheck (prop_monoidRightIdentity :: TwoS -> Bool)
    quickCheck (prop_FunctorIdentity :: TwoSI -> Bool)
    quickCheck (prop_FunctorCompose' :: TwoCompose)
    quickCheck (prop_foldr :: TwoFoldr)
    let appWitness :: Two (Sum Int) SSI
        appWitness = undefined
    quickBatch $ applicative appWitness
    let foldableWitness :: Two Int (String, String, String, Int, Int)
        foldableWitness = undefined
    quickBatch $ foldable foldableWitness
    let traversableWitness :: Two Int (Int, Int, [Int])
        traversableWitness = undefined
    quickBatch $ traversable traversableWitness