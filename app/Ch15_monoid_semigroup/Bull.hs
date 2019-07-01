module Ch15_monoid_semigroup.Bull where

import Test.QuickCheck
import Instances_Exercises.Properties.Semigroup
import Instances_Exercises.Properties.Monoid

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = oneof [return Fools, return Twoo]

instance Semigroup Bull where
    (<>) _ _ = Fools

instance Monoid Bull where
    mempty = Fools
    mappend = (<>)

type BullAssoc = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
    let sa = prop_semigroupAssoc
        mli = prop_monoidLeftIdentity
        mlr = prop_monoidRightIdentity
    quickCheck (sa :: BullAssoc)
    verboseCheck (mli :: Bull -> Bool)
    verboseCheck (mlr :: Bull -> Bool)