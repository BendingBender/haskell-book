module Instances_Exercises.Properties.Semigroup where

prop_semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
prop_semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)