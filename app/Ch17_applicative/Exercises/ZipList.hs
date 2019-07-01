module Ch17_applicative.Exercises.ZipList where

import GHC.Base (liftA2)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Instances_Exercises.List (List (Nil), zipWith', repeat', take')

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Semigroup a => Semigroup (ZipList' a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList' a) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure a = ZipList' $ repeat' a
    (<*>) (ZipList' Nil) _ = ZipList' Nil
    (<*>) _ (ZipList' Nil) = ZipList' Nil
    (<*>) (ZipList' fs) (ZipList' as) = ZipList' $ zipWith' (\f a -> f a) fs as

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary
    
instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' = let (ZipList' l) = xs
                  in take' 3000 l
            ys' = let (ZipList' l) = ys
                  in take' 3000 l

type SSI = (String, String, Int)

main :: IO ()
main = do
    let 
        zm :: ZipList' (Sum Int)
        zm = undefined
        za :: ZipList' SSI
        za = undefined
    quickBatch $ monoid zm
    quickBatch $ applicative za