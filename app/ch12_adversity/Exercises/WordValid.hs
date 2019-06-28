module WordValid where

import Lib

newtype Word' = Word' String deriving (Eq, Show)
data VowelsCons = VowelsCons Integer Integer deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord "" = Nothing
mkWord str = go (VowelsCons 0 0) str
    where
        go (VowelsCons vowels cons) "" =
            if vowels <= cons then Just (Word' str) else Nothing
        go (VowelsCons vowels cons) (c:str') =
            if isVovel c
                then go (VowelsCons (1 + vowels) cons) str'
                else go (VowelsCons vowels (1 + cons)) str'

isVovel :: Char -> Bool
isVovel c = any (\c' -> c' == c) "aeiou"

main :: IO ()
main = do
    test "mkWord \"a\"" (mkWord "a") Nothing
    test "mkWord \"arbitrary\"" (mkWord "arbitrary") (Just (Word' "arbitrary"))