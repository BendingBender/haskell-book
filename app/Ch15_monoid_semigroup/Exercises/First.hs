module Ch15_monoid_semigroup.Exercises.First where

import Test.QuickCheck
import Instances_Exercises.Properties.Semigroup
import Instances_Exercises.Properties.Monoid
import Ch15_monoid_semigroup.Exercises.Optional (Optional (Nada, Only))

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
    (<>) nada@(First' Nada) (First' Nada) = nada
    (<>) (First' Nada) only@(First' (Only _)) = only
    (<>) only@(First' (Only _)) (First' Nada) = only
    (<>) only@(First' (Only _)) (First' (Only _)) = only

instance Monoid (First' a) where
    mempty = First' Nada
    mappend = (<>)

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        oneof [return $ First' Nada, return $ First' $ Only a]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstAssoc =
       First' String
    -> First' String
    -> First' String
    -> Bool

type FstId =
    First' String -> Bool

main :: IO ()
main = do
    quickCheck (prop_semigroupAssoc :: FirstAssoc)
    quickCheck (prop_monoidLeftIdentity :: FstId)
    quickCheck (prop_monoidRightIdentity :: FstId)