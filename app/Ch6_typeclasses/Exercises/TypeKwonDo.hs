module Ch6_typeclasses.Exercises.TypeKwonDo where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB x y = (aToB x) == y

arith :: Num b
    => (a -> b)
    -> Integer
    -> a
    -> b
arith aToB x y = (fromInteger x) + (aToB y)