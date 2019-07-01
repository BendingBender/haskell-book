module Ch16_functor.Exercises.Possibly where

import Test.QuickCheck
import Instances_Exercises.Properties.Functor

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap f (Yeppers a) = Yeppers $ f a
    fmap _ LolNope = LolNope

instance (Arbitrary a) => Arbitrary (Possibly a) where
    arbitrary = do
        a <- arbitrary
        oneof [return LolNope, return $ Yeppers a]

type PossiblyI = Possibly Int
type PossiblyCompose = Fun Int Int -> Fun Int Int -> PossiblyI -> Bool

main :: IO ()
main = do
    quickCheck (prop_FunctorIdentity :: PossiblyI -> Bool)
    quickCheck (prop_FunctorCompose' :: PossiblyCompose)