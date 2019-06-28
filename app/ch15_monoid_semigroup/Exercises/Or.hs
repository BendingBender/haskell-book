module Or where

import Test.QuickCheck
import Data.Semigroup
import Instance_Exercises.Properties.Semigroup

data Or a b = Fst a | Snd b deriving (Show, Eq)

instance Semigroup (Or a b) where
    (Fst _) <> s@(Snd _) = s
    s@(Snd _) <> (Fst _) = s
    s@(Snd _) <> (Snd _) = s
    (Fst _) <> f@(Fst _) = f

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Fst a, return $ Snd b]

type OrAssoc =
    Or Int Int -> Or Int Int -> Or Int Int -> Bool

main :: IO ()
main =
    quickCheck (prop_semigroupAssoc :: OrAssoc)