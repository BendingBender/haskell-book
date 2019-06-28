module Natural where

import Lib

data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger n' = go n' 0
    where
        go Zero n = n
        go (Succ nat) n = go nat (n + 1)


integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i < 0 = Nothing
    | otherwise = Just (go Zero i)
    where
        go nat 0 = nat
        go nat i' = go (Succ nat) (i' - 1)

main :: IO ()
main = do
    test "natToInteger Zero" (natToInteger Zero) 0
    test "natToInteger (Succ Zero)" (natToInteger (Succ Zero)) 1
    test "natToInteger (Succ (Succ Zero))" (natToInteger (Succ (Succ Zero))) 2
    test "integerToNat 0" (integerToNat 0) (Just Zero)
    test "integerToNat 1" (integerToNat 1) (Just (Succ Zero))
    test "integerToNat 2" (integerToNat 2) (Just (Succ (Succ Zero)))
    test "integerToNat (-1)" (integerToNat (-1)) Nothing
