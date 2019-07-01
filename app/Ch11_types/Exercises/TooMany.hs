{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Ch11_types.Exercises.TooMany where

import Lib

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
    tooMany (n, _) = n > 43

-- instance TooMany (Int, Int) where
--     tooMany (n, n') = tooMany (n + n')

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (n, n') = tooMany (n + n')


main :: IO ()
main = do
    printTest "tooMany (Goats 42)" (tooMany (Goats 42))
    printTest "tooMany (44, \"Goats\")" (tooMany (44::Int, "Goats"))
    --printTest "tooMany (1, 42)" (tooMany (1::Int, 42::Int))
    printTest "tooMany (1, 42)" (tooMany (1::Int, 42::Int))
