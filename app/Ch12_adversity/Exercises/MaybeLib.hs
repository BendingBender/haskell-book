module Ch12_adversity.Exercises.MaybeLib where

import Lib

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing m = not (isJust m)

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ aToB (Just a) = aToB a

fromMaybe :: a -> Maybe a -> a
fromMaybe a m = mayybee a id m

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes list = go [] list
    where
        go cms [] = cms
        go cms (Nothing:ms) = go cms ms
        go cms (Just a:ms) = (a:go cms ms)


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe mas = go (Just []) mas
    where
        go Nothing _ = Nothing -- for the sake of completeness, can't actually happen
        go _ (Nothing:_) = Nothing
        go (Just as) [] = Just (reverse as)
        go (Just as) (Just a:mas') = go (Just (a:as)) mas'

main :: IO ()
main = do
    test "isJust (Just 1)" (isJust (Just (1::Int))) True
    test "isJust (Nothing)" (isJust (Nothing)) False
    test "isNothing (Just 1)" (isNothing (Just (1::Int))) False
    test "isNothing (Nothing)" (isNothing (Nothing)) True
    test "mayybee 0 (+1) Nothing" (mayybee (0::Int) (+(1::Int)) Nothing) 0
    test "mayybee 0 (+1) (Just 1)" (mayybee (0::Int) (+1) (Just (1::Int))) 2
    test "fromMaybe 0 Nothing" (fromMaybe (0::Int) Nothing) 0
    test "fromMaybe 0 (Just 1)" (fromMaybe (0::Int) (Just 1)) 1
    test "listToMaybe [1, 2, 3]" (listToMaybe ([1, 2, 3]::[Int])) (Just 1)
    test "listToMaybe []" (listToMaybe ([]::[Int])) (Nothing)
    test "maybeToList (Just 1)" (maybeToList (Just (1::Int))) [1]
    test "maybeToList Nothing" (maybeToList Nothing) ([]::[Int])
    test
        "catMaybes [Just 1, Nothing, Just 2]"
        (catMaybes [Just (1::Int), Nothing, Just 2])
        [1, 2]
    test
        "catMaybes . take 3 $ repeat Nothing"
        (catMaybes . take 3 $ repeat Nothing)
        ([]::[Int])
    test
        "flipMaybe [Just 1, Just 2, Just 3]"
        (flipMaybe [Just 1, Just 2, Just (3::Int)])
        (Just [1, 2, 3])
    test
        "flipMaybe [Just 1, Nothing, Just 3]"
        (flipMaybe [Just 1, Nothing, Just (3::Int)])
        Nothing
