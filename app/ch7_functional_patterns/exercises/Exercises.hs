module Exercises7 where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where
        xLast = fst (divMod x 10)
        d = snd (divMod xLast 10)

hunsD :: Integral a => a -> a
hunsD x = d
    where
        xLast = fst (divMod x 100)
        d = snd (divMod xLast 10)


foldBool :: a -> a -> Bool -> a
foldBool x y useLast =
    case useLast of
        False -> x
        True -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y useLast
    | useLast = y
    | otherwise = x

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)