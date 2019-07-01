module Ch12_adversity.Exercises.EitherLib where

import Lib

lefts' :: [Either a b] -> [a]
lefts' = foldr addLeft []
    where
        addLeft (Right _) as = as
        addLeft (Left a) as = (a:as)

rights' :: [Either a b] -> [b]
rights' = foldr addRight []
    where
        addRight (Left _) bs = bs
        addRight (Right b) bs = (b:bs)

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' bToC (Right b) = Just (bToC b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' aToC _ (Left a) = aToC a
either' _ bToC (Right b) = bToC b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' bToC = either' (\_ -> Nothing) (\b -> Just (bToC b))

main :: IO ()
main = do
    test
        "lefts' [Left 1, Right 2, Left 3]"
        (lefts' ([Left 1, Right 2, Left 3]::[Either Int Int]))
        [1,3]
    test
        "rights' [Right 1, Left 2, Right 3]"
        (rights' ([Right 1, Left 2, Right 3]::[Either Int Int]))
        [1,3]
    test
        "partitionEithers' [Right 1, Left 2, Right 3, Left 4]"
        (partitionEithers' ([Right 1, Left 2, Right 3, Left 4]::[Either Int Int]))
        ([2,4],[1,3])
    test
        "eitherMaybe' (+1) (Left 1)"
        (eitherMaybe' (+1) ((Left 1)::Either Int Int))
        Nothing
    test
        "eitherMaybe' (+1) (Right 2)"
        (eitherMaybe' (+1) ((Right 2)::Either Int Int))
        (Just 3)
    test
        "either' (+1) (subtract 1) (Left 1)"
        (either' (+1) (subtract 1) ((Left 1)::Either Int Int))
        2
    test
        "either' (+1) (subtract 1) (Right 2)"
        (either' (+1) (subtract 1) ((Right 2)::Either Int Int))
        1
    test
        "eitherMaybe'' (+1) (Left 1)"
        (eitherMaybe'' (+1) ((Left 1)::Either Int Int))
        Nothing
    test
        "eitherMaybe'' (+1) (Right 2)"
        (eitherMaybe'' (+1) ((Right 2)::Either Int Int))
        (Just 3)