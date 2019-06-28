module Instances_Exercises.Properties.Foldable where

import Data.Monoid
import Test.QuickCheck

prop_foldr :: (Foldable t, Eq b, Show b) => Fun (a, b) b -> b -> t a -> Property
prop_foldr fun z t =
    (foldr f z t) === (appEndo (foldMap (Endo . f) t) z)
    where
        f = applyFun2 fun