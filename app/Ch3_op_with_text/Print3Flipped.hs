-- Print3Flipped.hs
module Ch3_op_with_text.Print3Flipped where

myGreeting :: String
myGreeting = (++) "hello" " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where
        secondGreeting = (++) hello ((++) " " world)