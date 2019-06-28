module Database where

import Lib
import Data.Time

data DatabaseItem =
    DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ 
        DbDate (
            UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)
        ),
        DbNumber 9001,
        DbString "Hello, world!",
        DbDate (
            UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)
        )
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr dateFilter [] xs
    where
        dateFilter :: DatabaseItem -> [UTCTime] -> [UTCTime]
        dateFilter (DbDate x) xs' = x:xs'
        dateFilter _ xs' = xs'

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr numberFilter [] xs
    where
        numberFilter :: DatabaseItem -> [Integer] -> [Integer]
        numberFilter (DbNumber x) xs' = x:xs'
        numberFilter _ xs' = xs'

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr max (head dates) dates
        where dates = filterDbDate xs

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral . sumDb $ xs) / (fromIntegral . length . filterDbNumber $ xs)


main :: IO ()
main = do
    printTest "filterDbDate theDatabase" (filterDbDate theDatabase)
    printTest "filterDbNumber theDatabase" (filterDbNumber theDatabase)
    printTest "mostRecent theDatabase" (mostRecent theDatabase)
    printTest "sumDb theDatabase" (sumDb theDatabase)
    printTest "avgDb theDatabase" (avgDb theDatabase)

