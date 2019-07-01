module Ch20_foldable.Library where

import Data.Monoid (Any(..), Sum(..), Product(..))
import Data.Semigroup

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

sum :: (Foldable t, Num a) => t a -> a
sum =
    getSum . foldMap Sum

sum' :: (Foldable t, Num a) => t a -> a
sum' =
    foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product =
    getProduct . foldMap Product

product' :: (Foldable t, Num a) => t a -> a
product' =
    foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a =
    getAny . foldMap (Any . ((==) a))

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a =
    getAny . foldr (\value acc -> acc <> Any (value == a)) mempty

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum =
    fmap getMin . foldr (\curr acc -> fmap (<> Min curr) acc) Nothing

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum =
    fmap getMax . foldr (\curr acc -> fmap (<> Max curr) acc) Nothing

null :: (Foldable t) => t a -> Bool
null =
    foldr (const $ const False) True

length :: (Foldable t) => t a -> Int
length =
    foldr (const . (+) $ 1) 0

toList :: (Foldable t) => t a -> [a]
toList =
    foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold =
    foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' toMonoid =
    foldr (mappend . toMonoid) mempty