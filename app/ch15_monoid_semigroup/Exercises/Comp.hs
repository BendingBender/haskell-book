module Comp where

import Test.QuickCheck
import Data.Semigroup

newtype Comp a =
    Comp { unComp :: (a -> a) }

instance Show (Comp a) where
    show _ = "Comp {unComp = a -> a}"

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = do
        f <- arbitrary
        return $ Comp f

instance Semigroup (Comp a) where
    (Comp f) <> (Comp f') = Comp (f' . f)

instance Monoid (Comp a) where
    mempty = Comp id
    mappend = (<>)

type CI = Comp Int
type CompAssoc = CI -> CI -> CI -> Int -> Bool

prop_CompSemigroupAssoc :: Eq a => Comp a -> Comp a -> Comp a -> a -> Bool
prop_CompSemigroupAssoc c c' c'' a =
    (unComp (c <> (c' <> c'')) $ a) == (unComp ((c <> c') <> c'') $ a)

prop_CompMonoidLeftIdentity :: Eq a => Comp a -> a -> Bool
prop_CompMonoidLeftIdentity c a = (unComp (mempty <> c) $ a) == (unComp c $ a)

prop_CompMonoidRightIdentity :: Eq a => Comp a -> a -> Bool
prop_CompMonoidRightIdentity c a = (unComp (c <> mempty) $ a) == (unComp c $ a)

main :: IO ()
main = do
    quickCheck (prop_CompSemigroupAssoc :: CompAssoc)
    quickCheck (prop_CompMonoidLeftIdentity :: CI -> Int -> Bool)
    quickCheck (prop_CompMonoidRightIdentity :: CI -> Int -> Bool)