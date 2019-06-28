module Trivial where

import Test.QuickCheck
import Data.Semigroup
import Instance_Exercises.Properties.Semigroup
import Instance_Exercises.Properties.Monoid

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc =
    Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
    quickCheck (prop_semigroupAssoc :: TrivAssoc)
    quickCheck (prop_monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (prop_monoidRightIdentity :: Trivial -> Bool)