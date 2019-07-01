module Ch11_types.Records where

    data Person =
        Person { 
            name :: String,
            age :: Int }
            deriving (Eq, Show)

    data Person2 =
        Person2 { 
            name2 :: Bool,
            age2 :: Int }
            deriving (Eq, Show)