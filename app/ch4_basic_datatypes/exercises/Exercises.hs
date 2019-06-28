module Exercises4 where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then (-x) else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)
g :: Foldable f => f a -> Int
g xs = w `x` 1
    where w = length xs

y :: a -> a
y x = x

h :: (a, b) -> a
h (a, b) = a