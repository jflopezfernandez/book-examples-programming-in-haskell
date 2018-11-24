
module Main where

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n)    = n
eval (Add x y)  = eval x + eval y

type Stack = [Int]

data Op = PUSH Int
        | ADD
    deriving (Show)

type Code = [Op]

exec :: Code -> Stack -> Stack
exec [] s                       = s
exec (PUSH n : c) s             = exec c (n     : s)
exec (ADD    : c) (m : n : s)   = exec c (n + m : s)
exec _ _                        = error "Unknow input"

comp :: Expr -> Code
comp (Val n)    = [PUSH n]
comp (Add x y)  = comp x ++ comp y ++ [ADD]

main :: IO ()
main =
    let
        e = Add (Add (Val 2) (Val 3)) (Val 4)
    in do
        print (eval e)
        print (comp e)
