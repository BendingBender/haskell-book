module Ch9_lists.Exercises.Split where

split :: Char -> String -> [String]
split splitChar str = go str
    where
        go [] = []
        go toSplit = word:go remaining
            where
                word = takeWhile (/=splitChar) toSplit
                remaining = dropWhile (==splitChar) . dropWhile (/=splitChar) $ toSplit