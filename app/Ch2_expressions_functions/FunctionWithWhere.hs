-- FunctionWithWhere.hs
module Ch2_expressions_functions.FunctionWithWhere where

printInc :: (Show a, Num a) => a -> IO ()
printInc n = print plusTwo
    where plusTwo = n + 2
