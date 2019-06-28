module BoolDisj where

import Test.QuickCheck
import Data.Semigroup
import Instance_Exercises.Properties.Semigroup
import Instance_Exercises.Properties.Monoid

newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where
    (BoolDisj b) <> (BoolDisj b') = BoolDisj $ b || b'

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = do
        b <- arbitrary
        return $ BoolDisj b

type BoolDisjAssoc =
    BoolDisj -> BoolDisj -> BoolDisj -> Bool

main :: IO ()
main = do
    quickCheck (prop_semigroupAssoc :: BoolDisjAssoc)
    quickCheck (prop_monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (prop_monoidRightIdentity :: BoolDisj -> Bool)