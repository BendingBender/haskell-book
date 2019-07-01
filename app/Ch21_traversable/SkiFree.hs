module Ch21_traversable.SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S na a) = S (f <$> na) $ f a

instance Foldable n => Foldable (S n) where
    foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
    traverse f (S na a) = S <$> traverse f na <*> f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
    (=-=) = eq

main :: IO ()
main = do
    let witness :: S [] (String, String, Int)
        witness = undefined
    quickBatch $ functor witness
    let foldableWitness :: S [] (String, String, String, Int, Int)
        foldableWitness = undefined
    quickBatch $ foldable foldableWitness
    let traversableWitness :: S [] (Int, Int, [Int])
        traversableWitness = undefined
    quickBatch $ traversable traversableWitness

    -- sample' (arbitrary :: Gen (S [] Int))