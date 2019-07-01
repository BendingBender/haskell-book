module Ch15_monoid_semigroup.Exercises.Combine where

import Test.QuickCheck
import Data.Monoid (Sum)

newtype Combine a b =
    Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
    show _ = "Combine {unCombine = a -> b}"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        f <- arbitrary
        return $ Combine f

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine f') = Combine (\a -> f a <> f' a)

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine (\_ -> mempty)
    mappend = (<>)

type CI = Combine Int (Sum Int)
type CombineAssoc = CI -> CI -> CI -> Int -> Bool

prop_CombineSemigroupAssoc :: (Eq b, Semigroup b)
    => Combine a b -> Combine a b -> Combine a b -> a -> Bool
prop_CombineSemigroupAssoc c c' c'' x =
    (unCombine (c <> (c' <> c'')) $ x) == (unCombine ((c <> c') <> c'') $ x)

prop_CombineMonoidLeftIdentity ::
    (Eq b, Semigroup b, Monoid b) => Combine a b -> a -> Bool
prop_CombineMonoidLeftIdentity c a =
    (unCombine (mempty <> c) $ a) == (unCombine c $ a)

prop_CombineMonoidRightIdentity ::
    (Eq b, Semigroup b, Monoid b) => Combine a b -> a -> Bool
prop_CombineMonoidRightIdentity c a =
    (unCombine (c <> mempty) $ a) == (unCombine c $ a)

main :: IO ()
main = do
    quickCheck (prop_CombineSemigroupAssoc :: CombineAssoc)
    quickCheck (prop_CombineMonoidLeftIdentity :: CI -> Int -> Bool)
    quickCheck (prop_CombineMonoidRightIdentity :: CI -> Int -> Bool)