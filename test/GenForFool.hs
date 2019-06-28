module GenForFool where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof, frequency)

data Fool = Fulse | Frue deriving (Eq, Show)
data Fool' = Fulse' | Frue' deriving (Eq, Show)

instance Arbitrary Fool where
    arbitrary = oneof [return Fulse, return Frue]

instance Arbitrary Fool' where
    arbitrary = frequency [(2, return Fulse'), (1, return Frue')]