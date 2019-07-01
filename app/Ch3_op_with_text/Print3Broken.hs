-- Print3Broken.hs
module Ch3_op_with_text.Print3Broken where

printSecond :: String -> IO ()
printSecond greeting = do
    putStrLn greeting

main :: IO ()
main = do
    putStrLn greeting
    printSecond greeting
    where
        greeting = "Yarrrrr"