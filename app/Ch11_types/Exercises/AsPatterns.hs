module Ch11_types.Exercises.AsPatterns where

import Lib
import Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf subStr str =
    [] == (foldl reducer subStr str)
    where
        reducer [] _ = []
        reducer bs@(b:bs') a = if b == a then bs' else bs

capitalizeWords :: String -> [(String, String)]
capitalizeWords str =
    map (\word@(first:rest) -> (word, (toUpper first:rest))) (words str)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord word@(first:rest)
    | first >= 'a' && first <= 'z' = (chr (ord first - capitalOffset):rest)
    | otherwise = word
    where
        capitalOffset = ord 'a' - ord 'A'

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph ps =
    foldr (joinRight " ") "" . map (capitalizeWord . trimStart) . foldl paragraphs [] $ ps
    where
        paragraphs [] c = paragraphs [""] c
        paragraphs (curr:rest) c
            | c == '.' = ("":((curr ++ "."):rest))
            | otherwise = ((curr ++ [c]):rest)
        trimStart "" = ""
        trimStart str@(c:rest)
            | c == ' ' = trimStart rest
            | otherwise = str
        joinRight _ joined "" = joined
        joinRight jStr joined str
            | joined == "" = str
            | otherwise = str ++ jStr ++ joined


main :: IO ()
main = do
    test
        "isSubseqOf \"blah\" \"blahwoot\""
        (isSubseqOf "blah" "blahwoot")
        True
    test
        "isSubseqOf \"blah\" \"wootblah\""
        (isSubseqOf "blah" "wootblah")
        True
    test
        "isSubseqOf \"blah\" \"wboloath\""
        (isSubseqOf "blah" "wboloath")
        True
    test
        "isSubseqOf \"blah\" \"wootbla\""
        (isSubseqOf "blah" "wootbla")
        False
    test
        "isSubseqOf \"blah\" \"halbwoot\""
        (isSubseqOf "blah" "halbwoot")
        False
    test
        "isSubseqOf \"blah\" \"blawhoot\""
        (isSubseqOf "blah" "blawhoot")
        True
    test
        "capitalizeWords \"hello world\""
        (capitalizeWords "hello world")
        [("hello", "Hello"), ("world", "World")]
    test "capitalizeWord \"Chortle\"" (capitalizeWord "Chortle") "Chortle"
    test "capitalizeWord \"chortle\"" (capitalizeWord "chortle") "Chortle"
    test
        "capitalizeParagraph \"blah. woot ha.\""
        (capitalizeParagraph "blah. woot ha.")
        "Blah. Woot ha."