module Ch9_lists.Exercises.PoemLines where

import Ch9_lists.Exercises.Split (split)

firstSen :: String
firstSen = "Tyger Tyger, burning bright\n"
secondSen :: String
secondSen = "In the forests of the night\n"
thirdSen :: String
thirdSen = "What immortal hand or eye\n"
fourthSen :: String
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences :: String
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

myLines :: String -> [String]
myLines = split '\n'

-- What we want 'myLines sentences'
-- to equal
shouldEqual :: [String]
shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main = print $
    "Are they equal? "
    ++ show (myLines sentences == shouldEqual)