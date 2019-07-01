module Ch3_op_with_text.Exercises.Exercises where

-- ch 3.8
emphasis :: String -> String
emphasis s = s ++ "!"

fifthLetter :: String -> String
fifthLetter s = take 1 $ drop 4 s

tenthOnwards :: String -> String
tenthOnwards = drop 9

thirdChar :: String -> Char
thirdChar = (!! 2)

nthCharOfCurry :: Int -> Char
nthCharOfCurry = ("Curry is awesome" !!)