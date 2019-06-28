module Instances_Exercises.Four where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Semigroup
import Instances_Exercises.Properties.Semigroup
import Instances_Exercises.Properties.Functor

data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
    => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four a' b' c' d') =
        Four (a <> a') (b <> b') (c <> c') (d <> d')

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c $ f d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure c = Four mempty mempty mempty c
    (<*>) (Four a b c f) (Four a' b' c' d) =
        Four (a `mappend` a') (b `mappend` b') (c `mappend` c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

type FourS = Four String String String String
type FourSI = Four String String String Int
type FourAssoc = FourS -> FourS -> FourS -> Bool
type FourCompose = Fun Int Int -> Fun Int Int -> FourSI -> Bool
type SSI = (String, String, Int)

main :: IO ()
main = do
    quickCheck (prop_semigroupAssoc :: FourAssoc)
    quickCheck (prop_FunctorIdentity :: FourSI -> Bool)
    quickCheck (prop_FunctorCompose' :: FourCompose)
    let f :: Four (Sum Int) (Product Int) String SSI
        f = undefined
    quickBatch $ applicative f
