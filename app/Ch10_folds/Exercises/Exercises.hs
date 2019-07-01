module Ch10_folds.Exercises.Exercises where

import Lib

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

nouns :: [String]
nouns = ["person", "animal", "name", "thing", "idea"]

verbs :: [String]
verbs = ["run", "hear", "call", "know", "recognize"]

comb3 :: [a] -> [b] -> [(a, b, a)]
comb3 as bs = [(a, b, a') | a <- as, b <- bs, a' <- as]

svs :: String -> String -> [(Char, Char, Char)]
svs ss vs = comb3 ss vs

nvn :: [String] -> [String] -> [(String, String, String)]
nvn ns vs = comb3 ns vs

svsByFirst :: Char -> String -> String -> [(Char, Char, Char)]
svsByFirst first ss vs = filter ((==) first . fst3) (svs ss vs)
    where fst3 (x, _, _) = x

-- calcs floored average word length in a sentence
seekritFunc :: String -> Int
seekritFunc x =
    div (sum . map length . words $ x) (length . words $ x)

seekritFuncFrac :: String -> Double
seekritFuncFrac x =
    (/) (fromIntegral . sum . map length . words $ x) (fromIntegral . length . words $ x)

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
--myAny f = foldr (\a b -> (||) b . f $ a) False
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> b || (x == a)) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = myAny (==x)

myReverse :: [a] -> [a]
myReverse = foldl (\b a -> a:b) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> (f a):b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a:b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMinMaxBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myMinMaxBy o f = get . foldr comp Nothing
    where
        comp a Nothing = Just a
        comp a (Just b) = if f a b == o then Just a else Just b
        get Nothing = undefined
        get (Just x) = x

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myMinMaxBy GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myMinMaxBy LT


main :: IO ()
main = do
    printTest "svsByFirst 'p' stops vowels" (svsByFirst 'p' stops vowels)
    printTest "take 10 (nvn nouns verbs)" (take 10 (nvn nouns verbs))
    printTest
        "seekritFunc \"The quick brown fox jumps over the lazy dog\""
        (seekritFunc "The quick brown fox jumps over the lazy dog")
    printTest
        "seekritFuncFrac \"The quick brown fox jumps over the lazy dog\""
        (seekritFuncFrac "The quick brown fox jumps over the lazy dog")
    printTest "myOr [False]" (myOr [False])
    printTest "myOr [True, False]" (myOr [True, False])
    printTest "myAny even [1, 3, 5]" (myAny even ([1, 3, 5] :: [Int]))
    printTest "myAny odd [1, 3, 5]" (myAny odd ([1, 3, 5] :: [Int]))
    printTest "myElem 1 [1..10]" (myElem 1 ([1..10] :: [Int]))
    printTest "myElem 1 [2..10]" (myElem 1 ([2..10] :: [Int]))
    printTest "myElem2 1 [1..10]" (myElem2 1 ([1..10] :: [Int]))
    printTest "myElem2 1 [2..10]" (myElem2 1 ([2..10] :: [Int]))
    printTest "myReverse \"blah\"" (myReverse "blah")
    printTest "myReverse [1..5]" (myReverse ([1..5] :: [Int]))
    printTest "myMap (+1) [1..5]" (myMap (+1) ([1..5] :: [Int]))
    printTest "myFilter (>2) [1..5]" (myFilter (>2) ([1..5] :: [Int]))
    printTest "squish [[1..5],[6..10]]" (squish ([[1..5],[6..10]] :: [[Int]]))
    printTest "squishMap (\\x -> [1, x, 3]) [2]" (squishMap (\x -> [1, x, 3]) ([2] :: [Int]))
    printTest
        "squishMap (\\x -> \"WO \" ++ [x] ++ \" OT \") \"blah\""
        (squishMap (\x -> "WO " ++ [x] ++ " OT ") "blah")
    printTest "squishAgain [[1..5],[6..10]]" (squishAgain ([[1..5],[6..10]] :: [[Int]]))
    printTest "myMaximumBy (\\_ _ -> GT) [1..10]" (myMaximumBy (\_ _ -> GT) ([1..10] :: [Int]))
    printTest "myMaximumBy (\\_ _ -> LT) [1..10]" (myMaximumBy (\_ _ -> LT) ([1..10] :: [Int]))
    printTest "myMaximumBy compare [1..10]" (myMaximumBy compare ([1..10] :: [Int]))
    printTest "myMinimumBy (\\_ _ -> GT) [1..10]" (myMinimumBy (\_ _ -> GT) ([1..10] :: [Int]))
    printTest "myMinimumBy (\\_ _ -> LT) [1..10]" (myMinimumBy (\_ _ -> LT) ([1..10] :: [Int]))
    printTest "myMinimumBy compare [1..10]" (myMinimumBy compare ([1..10] :: [Int]))
    