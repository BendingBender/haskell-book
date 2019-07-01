module Ch8_recursion.Exercises.Recursion where

import Test.Hspec

func :: [a] -> [a] -> [a]
func x y = x ++ y

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo n = go n 0
    where go currN currSum
            | currN == 0 = currSum
            | otherwise = go (currN - 1) (currSum + currN)

mulRec :: Integral a => a -> a -> a
mulRec x y = go y' 0
    where
        x' = if y < 0 then (-x) else x
        y' = abs y
        go n currSum
            | (x' == 0) || (y' == 0) = 0
            | n == 0 = currSum
            | otherwise = go (n - 1) (currSum + x')

data DividedResult a = Result (a, a) | DividedByZero deriving Show
dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy _ 0 = DividedByZero
dividedBy num denom = wrapResult . fixSign num denom . go absNum absDenom $ 0
    where
        absNum = abs num
        absDenom = abs denom
        wrapResult result = Result result
        fixSign n d result =
            if (((n < 0) && (d > 0)) || (d < 0 && n > 0))
                then (-(fst result), snd result)
                else result
        go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

mc91 :: Integral a => a -> a
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 (mc91 (n + 11))

main :: IO ()
main = hspec $ do
    describe "Recursion" $ do
        it "recursive 2 * 0 = 0" $ do
            mulRec 2 0 `shouldBe` (0::Integer)
        it "recursive 0 * 2 = 0" $ do
            mulRec 0 2 `shouldBe` (0::Integer)
        it "recursive 3 * 5 = 15" $ do
            mulRec 3 5 `shouldBe` (15::Integer)
        it "recursive 3 * -5 = -15" $ do
            mulRec 3 (-5) `shouldBe` ((-15)::Integer)
        it "recursive -3 * 5 = -15" $ do
            mulRec (-3) 5 `shouldBe` ((-15)::Integer)
        it "recursive -1 * -1 = 1" $ do
            mulRec (-1) (-1) `shouldBe` (1::Integer)