module Lib (
    printTest,
    test
    ) where


printTest :: Show a => String -> a -> IO ()
printTest desc textExpr = do
    putStrLn (desc ++ " ===> " ++ (show textExpr))

test :: (Show a, Eq a) => String -> a -> a -> IO ()
test desc textExpr expected =
    if textExpr == expected
        then putStrLn ("OK: " ++ desc ++ " == " ++ (show textExpr))
        else putStrLn (
            "FAIL: " ++ desc
            ++ "; expected -> " ++ (show expected)
            ++ "; got -> " ++ (show textExpr)
        )