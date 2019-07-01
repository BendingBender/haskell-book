module Ch6_typeclasses.Trivial where

data Trivial =
    Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True