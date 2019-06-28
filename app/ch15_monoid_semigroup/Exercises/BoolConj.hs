module BoolConj where

import Test.QuickCheck
import Data.Semigroup
import Instance_Exercises.Properties.Semigroup
import Instance_Exercises.Properties.Monoid

newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
    (BoolConj b) <> (BoolConj b') = BoolConj $ b && b'

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = do
        b <- arbitrary
        return $ BoolConj b

type BoolConjAssoc =
    BoolConj -> BoolConj -> BoolConj -> Bool

main :: IO ()
main = do
    quickCheck (prop_semigroupAssoc :: BoolConjAssoc)
    quickCheck (prop_monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (prop_monoidRightIdentity :: BoolConj -> Bool)