module Ch21_traversable.Tree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a
    = Empty
    | Leaf a
    | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node as a as') = Node (f <$> as) (f a) (f <$> as')

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node as a as') = foldMap f as <> f a <> foldMap f as'

    foldr _ initial Empty = initial
    foldr f initial (Leaf a) = f a initial
    foldr f initial (Node as a as') =
        foldr f currentFolded as
        where
            currentFolded = f a $ foldr f initial as'

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node as a as') = Node <$> traverse f as <*> f a <*> traverse f as'

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = do
        a <- arbitrary
        tree <- arbitrary
        tree' <- arbitrary
        oneof [return Empty, return $ Leaf a, return $ Node tree a tree']

instance Eq a => EqProp (Tree a) where
    (=-=) = eq

main :: IO ()
main = do
    tree <- sample' (arbitrary :: Gen (Tree Int))
    putStrLn . show $ tree
    let witness :: Tree (String, String, Int)
        witness = undefined
    quickBatch $ functor witness
    let foldableWitness :: Tree (String, String, String, Int, Int)
        foldableWitness = undefined
    quickBatch $ foldable foldableWitness
    let traversableWitness :: Tree (Int, Int, [Int])
        traversableWitness = undefined
    quickBatch $ traversable traversableWitness