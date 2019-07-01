module Ch11_types.Exercises.Phone where

import Data.List
import Data.Char
import Data.Maybe
import Lib

-- Valid presses: 1 and up
type Presses = Int
-- validButtons = "1234567890*#"
type Digit = Char
type LetterWithCount = (Char, Int)
type LetterWithCost = (Char, Int)
data Button = Button {
        buttonDigit :: Digit,
        buttonChar :: Char,
        buttonPresses :: Presses
    }
    deriving (Eq, Show)
data ShiftModifier = ShiftModifier {
        modifierDigit :: Digit,
        modifierPresses :: Presses
    }
    deriving (Eq, Show)

data DaPhone = DaPhone [Button] ShiftModifier deriving (Eq, Show)

daPhone :: DaPhone
daPhone =
    DaPhone [
        Button '1' '1' 1,
        Button '2' 'a' 1,
        Button '2' 'b' 2,
        Button '2' 'c' 3,
        Button '2' '2' 4,
        Button '3' 'd' 1,
        Button '3' 'e' 2,
        Button '3' 'f' 3,
        Button '3' '3' 4,
        Button '4' 'g' 1,
        Button '4' 'h' 2,
        Button '4' 'i' 3,
        Button '4' '4' 4,
        Button '5' 'j' 1,
        Button '5' 'k' 2,
        Button '5' 'l' 3,
        Button '5' '5' 4,
        Button '6' 'm' 1,
        Button '6' 'n' 2,
        Button '6' 'o' 3,
        Button '6' '6' 4,
        Button '7' 'p' 1,
        Button '7' 'q' 2,
        Button '7' 'r' 3,
        Button '7' 's' 4,
        Button '7' '7' 5,
        Button '8' 't' 1,
        Button '8' 'u' 2,
        Button '8' 'v' 3,
        Button '8' '8' 4,
        Button '9' 'w' 1,
        Button '9' 'x' 2,
        Button '9' 'y' 3,
        Button '9' 'z' 4,
        Button '9' '9' 5,
        Button '0' ' ' 1,
        Button '0' '0' 2,
        Button '*' '*' 2,
        Button '#' '.' 1,
        Button '#' ',' 2,
        Button '#' '#' 3
    ]
    (ShiftModifier '*' 1)

convo :: [String]
convo =
    [
        "Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Just making sure rofl ur turn"
    ]

buttonToTaps :: Maybe Button -> [(Digit, Presses)]
buttonToTaps Nothing = []
buttonToTaps (Just button) = [
        ((buttonDigit button), (buttonPresses button))
    ]

shiftButtonToTaps :: ShiftModifier -> Maybe Button -> [(Digit, Presses)]
shiftButtonToTaps _ Nothing = []
shiftButtonToTaps shift button = (
        ((modifierDigit shift), (modifierPresses shift))
        :
        (buttonToTaps button)
    )

buttonMatchesChar :: Char -> Button -> Bool
buttonMatchesChar char (Button _ c _) = char == c

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone@(DaPhone _ shift) c
    | isUpper c = shiftButtonToTaps shift (findButton phone (toLower c))
    | otherwise = buttonToTaps (findButton phone c)
    where
        findButton (DaPhone buttons _) char = find (buttonMatchesChar char) buttons

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead _ "" = []
cellPhonesDead phone msg = concat . map (reverseTaps phone) $ msg

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps [] = 0
fingerTaps taps = foldr (\(_, p) tapCount -> tapCount + p) 0 taps

mostPopularLetter :: String -> Maybe Char
mostPopularLetter = mostPopularFrom

mostPopularFrom :: Eq a => [a] -> Maybe a
mostPopularFrom [] = Nothing
mostPopularFrom xs =
    Just . fst . fromJust
    . foldr maxCount Nothing
    . foldr countEl [] $ xs
    where
        maxCount :: (Eq a) => (a, Int) -> Maybe (a, Int) -> Maybe (a, Int)
        maxCount newX Nothing = Just newX
        maxCount newX@(_, newXCount) (Just maxX@(_, maxXCount))
            | maxXCount >= newXCount = Just maxX
            | otherwise = Just newX
        countEl :: (Eq a) => a -> [(a, Int)] -> [(a, Int)]
        countEl x xCounts
            | any (\(x', _) -> x' == x) xCounts = incCountForEls x xCounts
            | otherwise = ((x, 1):xCounts)
        incCountForEls :: (Eq a) => a -> [(a, Int)] -> [(a, Int)]
        incCountForEls x xCounts = map (incCountForEl x) xCounts
        incCountForEl :: (Eq a) => a -> (a, Int) -> (a, Int)
        incCountForEl x tuple@(x', count)
            | x == x' = (x', count + 1)
            | otherwise = tuple

letterCost :: Char -> Presses
letterCost c = fingerTaps . reverseTaps daPhone $ c

mostPopLetterWithCost :: String -> Maybe LetterWithCost
mostPopLetterWithCost "" = Nothing
mostPopLetterWithCost msg = Just (mostPopLetter, letterCost mostPopLetter)
    where
        mostPopLetter = fromJust . mostPopularLetter $ msg

coolestLtr :: [String] -> Maybe Char
coolestLtr [] = Nothing
coolestLtr msgs = mostPopularLetter . concat . map mapper $ msgs
    where
        mapper "" = ""
        mapper msg
            | char == Nothing = ""
            | otherwise = [fromJust char]
            where
                char = mostPopularLetter msg

coolestWord :: [String] -> Maybe String
coolestWord msgs =
    mostPopularFrom . words . concat . map (\msg -> " " ++ msg) $ msgs

main :: IO ()
main = do
    printTest
        "cellPhonesDead daPhone (convo !! 0)"
        (cellPhonesDead daPhone (convo !! 0))
    printTest
        "fingerTaps (cellPhonesDead daPhone (convo !! 0))"
        (fingerTaps (cellPhonesDead daPhone (convo !! 0)))
    printTest
        "mostPopularLetter (convo !! 0)"
        (mostPopularLetter (convo !! 0))
    printTest
        "letterCost . fromJust . mostPopularLetter $ (convo !! 0)"
        (letterCost . fromJust . mostPopularLetter $ (convo !! 0))
    printTest
        "mostPopLetterWithCost (convo !! 0)"
        (mostPopLetterWithCost (convo !! 0))
    printTest
        "coolestLtr convo"
        (coolestLtr convo)
    printTest
        "coolestWord convo"
        (coolestWord convo)