module Ch18_monad.Exercises.PhhhbbtttEither where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data PhhhbbtttEither b a =
    Left' a
    | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
    fmap f (Left' a) = Left' $ f a
    fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
    pure = Left'
    Right' b <*> _ = Right' b
    _ <*> Right' b = Right' b
    Left' f <*> Left' a' = Left' $ f a'

instance Monad (PhhhbbtttEither b) where
    return = pure
    Right' b >>= _ = Right' b
    Left' a >>= f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Left' a, return $ Right' b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq

main :: IO ()
main = do
    let e :: PhhhbbtttEither Bool (String, String, Int)
        e = undefined
    quickBatch $ applicative e
    quickBatch $ monad e