module Ch9_lists.Exercises.Zipping where

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (\x x' -> (x, x'))

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (x':xs') = (f x x'):myZipWith f xs xs'

main :: IO ()
main = do
    print (myZip [1, 2, 3] [4, 5, 6])
    print (myZip [1, 2] [4, 5, 6])
    print (myZip [1, 2, 3] [4])
    print (zipWith max [10, 5, 34, 9] [6, 8, 12, 7])
    print (zipWith (==) ['a'..'f'] ['a'..'m'])