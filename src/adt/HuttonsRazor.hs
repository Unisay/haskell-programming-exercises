module HuttonsRazor where

data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add l r) = eval l + eval r

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add l r) = printExpr l ++ " + " ++ printExpr r
