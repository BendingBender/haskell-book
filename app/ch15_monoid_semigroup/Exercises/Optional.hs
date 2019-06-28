module Ch15_monoid_semigroup.Exercises.Optional where

import Data.Monoid
import Test.Hspec

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend (Only x) (Only x') = Only $ mappend x x'
    mappend Nada only@(Only _) = only
    mappend only@(Only _) Nada = only
    mappend Nada Nada = Nada

main :: IO ()
main = hspec $ do
    describe "Optional Monoid" $ do
        it "delegates combination to wrapped type" $ do
            (Only (Sum 1) `mappend` Only (Sum 1)) `shouldBe` Only (Sum (2::Integer))
            (Only (Product 4) `mappend` Only (Product 2)) `shouldBe` Only (Product (8::Integer))
        it "works with empty values" $ do
            (Only (Sum 1) `mappend` Nada) `shouldBe` Only (Sum (1::Integer))
            (Only [1] `mappend` Nada) `shouldBe` Only ([1]::[Integer])
            (Nada `mappend` Only (Sum 1)) `shouldBe` Only (Sum (1::Integer))