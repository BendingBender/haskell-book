{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-incomplete-patterns -Wno-name-shadowing #-}

module Ch5_types.Exercises.Exercises where

ex1a = (*9) 6 -- Num
ex1b = head [(0,"doge"),(1,"kitteh")] -- (Num, [Char])
ex1c = head [(0 :: Integer ,"doge"),(1,"kitteh")] -- (Integer, [Char])
ex1d = if False then True else False -- Bool
ex1e = length [1, 2, 3, 4, 5] -- Int
ex1f = (length [1, 2, 3, 4]) > (length "TACOCAT") -- Bool

x=5
y=x+5
w = y * 10 -- Num a => a
z y' = y' * 10 -- z :: Num a => a -> a
f=4/y -- Fractional a => a

x2 = "Julie"
y2 = " <3 "
z2 = "Haskell"
f2 = x2 ++ y2 ++ z2 -- [Char]


functionH :: [a] -> a
functionH (x':_) = x'

functionC :: Ord a => a -> a -> Bool
functionC x' y' =
    if (x' > y') then True else False

functionS :: (a, b) -> b
functionS (_, y') = y'

i :: a -> a
i x' = x'

c :: a -> b -> a
c x' _ = x'

c' :: a -> b -> b
c' _ y = y

r :: [a] -> [a]
r x' = tail x'

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)

a :: (a -> c) -> a -> a
a _ x' = x'

a' :: (a -> b) -> a -> b
a' aToB a'' = aToB a''