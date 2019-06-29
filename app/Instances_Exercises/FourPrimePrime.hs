module Instances_Exercises.FourPrimePrime where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four'' a b = Four'' a b b b deriving (Show, Eq)

instance Functor (Four'' a) where
    fmap f (Four'' a b b' b'') = Four'' a (f b) (f b') (f b'')

instance Monoid a => Applicative (Four'' a) where
    pure b = Four'' mempty b b b
    (<*>) (Four'' a fb fb' fb'') (Four'' a' b b' b'') = Four'' (a <> a') (fb b) (fb' b') (fb'' b'')

instance Foldable (Four'' a) where
    foldMap f (Four'' _ b b' b'') = f b <> f b' <> f b''
    foldr f initial (Four'' _ b b' b'') = f b . f b' . f b'' $ initial

instance Traversable (Four'' a) where
    traverse f (Four'' a b b' b'') = Four'' a <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four'' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        b'' <- arbitrary
        return $ Four'' a b b' b''

instance (Eq a, Eq b) => EqProp (Four'' a b) where
    (=-=) = eq

main :: IO ()
main = do
    let witness :: Four'' (Sum Int) (String, String, Int)
        witness = undefined
    quickBatch $ functor witness
    quickBatch $ applicative witness
    let foldableWitness :: Four'' Int (String, String, String, Int, Int)
        foldableWitness = undefined
    quickBatch $ foldable foldableWitness
    let traversableWitness :: Four'' Int (Int, Int, [Int])
        traversableWitness = undefined
    quickBatch $ traversable traversableWitness