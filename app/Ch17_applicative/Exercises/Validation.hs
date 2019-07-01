module Ch17_applicative.Exercises.Validation where

import Test.QuickCheck (Arbitrary, arbitrary, oneof)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
    pure = Success
    (<*>) (Success f) (Success a) = Success $ f a
    (<*>) (Failure e) (Failure e') = Failure $ e `mappend` e'
    (<*>) (Failure e) _ = Failure e
    (<*>) _ (Failure e) = Failure e

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        oneof [return $ Failure e, return $ Success a]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

type SSI = (String, String, Int)

main :: IO ()
main = do
    let v :: Validation (Sum Int) SSI
        v = undefined
    quickBatch $ applicative v