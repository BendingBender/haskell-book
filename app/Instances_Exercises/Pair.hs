module Instances_Exercises.Pair where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Instances_Exercises.Properties.Functor

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        return $ Pair a a'

instance (Eq a) => EqProp (Pair a) where
    (=-=) = eq

type PairI = Pair Int
type PairCompose = Fun Int Int -> Fun Int Int -> PairI -> Bool
type SSI = (String, String, Int)

main :: IO ()
main = do
    quickCheck (prop_FunctorIdentity :: PairI -> Bool)
    quickCheck (prop_FunctorCompose' :: PairCompose)
    let p :: Pair SSI
        p = undefined
    quickBatch $ applicative p