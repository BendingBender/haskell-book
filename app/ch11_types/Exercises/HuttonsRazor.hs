module HuttonsRazor where

import Lib

data Expr = Lit Integer | Add Expr Expr deriving Show

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add expr expr') = eval expr + eval expr'

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add expr expr') = printExpr expr ++ " + " ++ printExpr expr'

main :: IO ()
main = do
    test
        "eval (Add (Lit 1) (Lit 9001))"
        (eval (Add (Lit 1) (Lit 9001)))
        9002
    test
        "printExpr (Add (Lit 1) (Lit 9001))"
        (printExpr (Add (Lit 1) (Lit 9001)))
        "1 + 9001"
    test
        "printExpr (Add (Lit 1) (Add (Add (Lit 9001) (Lit 1)) (Lit 20001)))"
        (printExpr (Add (Lit 1) (Add (Add (Lit 9001) (Lit 1)) (Lit 20001))))
        "1 + 9001 + 1 + 20001"