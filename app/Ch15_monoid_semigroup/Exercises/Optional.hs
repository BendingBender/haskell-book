module Ch15_monoid_semigroup.Exercises.Optional where

import Data.Monoid
import Test.Hspec

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) (Only x) (Only x') = Only $ x <> x'
    (<>) Nada only@(Only _) = only
    (<>) only@(Only _) Nada = only
    (<>) Nada Nada = Nada

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend = (<>)

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