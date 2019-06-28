module Standard where

import Lib

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
    if x == False
        then False
        else myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
    if x == True
        then True
        else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny predicate (x:xs) =
    if (predicate x)
        then True
        else myAny predicate xs

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 predicate xs = or (map predicate xs)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) =
    if (x == y)
        then True
        else myElem y xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 y xs = any (\x -> x == y) xs

myReverse :: [a] -> [a]
myReverse xs = go [] xs
    where
        go revXs [] = revXs
        go revXs (x:xs') = go (x:revXs) xs'

squish :: [[a]] -> [a]
squish deepXs = go [] deepXs
    where
        go flatXs [] = flatXs
        go flatXs (xs:xsxs) = merge xs (go flatXs xsxs)
        merge [] flatXs = flatXs
        merge (x:xs) flatXs = x:(merge xs flatXs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish (map f xs)

squishAgain :: [[a]] -> [a]
squishAgain xsxs = squishMap (\x -> x) xsxs

myMinMaxBy :: Ordering -> (a -> a -> Ordering) -> [a] -> Maybe a
myMinMaxBy _ _ [] = Nothing
myMinMaxBy minMax f (x:xs) = Just (go x xs)
    where
        go max' [] = max'
        go max' (x':xs') = go (minMaxBy f max' x') xs'
        minMaxBy f' x' x'' = if (f' x' x'' == minMax)
            then x'
            else x''

myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy = myMinMaxBy GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMinimumBy = myMinMaxBy LT

myMaximum :: (Ord a) => [a] -> Maybe a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> Maybe a
myMinimum = myMinimumBy compare


main :: IO ()
main = do
    printTest "myOr []" (myOr [])
    printTest "myOr [False]" (myOr [False])
    printTest "myOr [False, True]" (myOr [False, True])
    printTest "myOr [True, False]" (myOr [True, False])
    printTest "myAny even [1, 3, 5]" (myAny even ([1, 3, 5] :: [Int]))
    printTest "myAny odd [1, 3, 5]" (myAny odd ([1, 3, 5] :: [Int]))
    printTest "myAny2 even [1, 3, 5]" (myAny2 even ([1, 3, 5] :: [Int]))
    printTest "myAny2 odd [1, 3, 5]" (myAny2 odd ([1, 3, 5] :: [Int]))
    printTest "myElem 1 [1..10]" (myElem 1 ([1..10] :: [Int]))
    printTest "myElem 1 [2..10]" (myElem 1 ([2..10] :: [Int]))
    printTest "myElem2 1 [1..10]" (myElem2 1 ([1..10] :: [Int]))
    printTest "myElem2 1 [2..10]" (myElem2 1 ([2..10] :: [Int]))
    printTest "myReverse \"blah\"" (myReverse "blah")
    printTest "myReverse [1..5]" (myReverse ([1..5] :: [Int]))
    printTest "squish [[1..5], [6..10]]" (squish ([[1..5], [6..10]] :: [[Int]]))
    printTest "squishMap (\\x -> [1, x, 3]) [2]" (squishMap (\x -> [1, x, 3]) ([2]::[Int]))
    printTest "squishMap (\\x -> \"WO \"++[x]++\" HOO \") \"123\"" (squishMap (\x -> "WO "++[x]++" HOO ") "123")
    printTest "squishAgain [[1..5], [6..10]]" (squishAgain ([[1..5], [6..10]] :: [[Int]]))
    printTest "myMaximumBy compare [1, 53, 9001, 10]" (myMaximumBy compare ([1, 53, 9001, 10] :: [Int]))
    printTest "myMinimumBy compare [1, 53, 9001, 10]" (myMinimumBy compare ([1, 53, 9001, 10] :: [Int]))
    printTest "myMaximum [1, 53, 9001, 10]" (myMaximum ([1, 53, 9001, 10] :: [Int]))
    printTest "myMinimum [1, 53, 9001, 10]" (myMinimum ([1, 53, 9001, 10] :: [Int]))
