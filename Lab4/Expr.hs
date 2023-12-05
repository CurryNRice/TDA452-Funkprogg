module Expr where

  
import Prelude hiding (sin, cos)
import qualified Prelude as P (sin, cos)


{-data Expr
  = Num Integer
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Sin Expr
  | Cos Expr
 deriving Eq
-}
{-
Borde kunna göra nåfot sånt här 
data Opr = Mul | Add
data Trig = Sin | Cos 
Då blir datatypen: 
data Expr
  = Num Number
  | X
  | Opr Expr Expr
  | Trig Expr
 deriving Eq
-}
data Func = Sin | Cos
 deriving (Eq, Show)

data Opr = Add | Mul
 deriving (Eq, Show)

data Expr
  = Num Double
  | X
  | Opr Opr Expr Expr 
  | Func Func Expr 
  -- | Cos Expr 
 deriving (Eq, Show)


simpEx = (add X X)

exExpr = mul (add (sin X) (sin X)) (num 2.3)
ex2Expr = mul (add (sin (mul (num 2) X)) (sin X)) (num 2.3)

x :: Expr
x = X 
num :: Double -> Expr
num d = Num d
add,mul :: Expr -> Expr -> Expr
add e1 e2 = Opr Add e1 e2
mul e1 e2 = Opr Mul e1 e2

sin,cos :: Expr -> Expr -- Ändra så dessa heter sin och cos, det finns ngot sätt lol
sin e = Func Sin e
cos e = Func Cos e

size :: Expr -> Int
size (Opr _ e1 e2) = 1 + size e1 + size e2
size (Func _ e1) = 1 + size e1
size _ = 0


showExpr :: Expr -> String
showExpr (Opr Mul X (Num n)) = showExpr (Num n) ++ "x"
showExpr (Opr Mul (Num n) X) = showExpr (Num n) ++ "x"

showExpr (Opr Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2
showExpr (Opr Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Func f e) = case f of Sin -> "sin"; Cos -> "cos" 
                        ++ "(" ++ showExpr e ++ ")"  
showExpr X = "x"
showExpr (Num n) = show n

showFactor (Opr Add e1 e2) = "("++(showExpr $ Opr Add e1 e2)++")"
showFactor e  = showExpr e

-- | Given an Expression and a value for X, it evaluates the result of the expression.

eval :: Expr -> Double -> Double
eval (Num n) x = n
eval (X) x     = x
eval (Opr Add e1 e2) x = (eval e1 x) + (eval e2 x) 
eval (Opr Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (Func Sin e1)   x = P.sin $ eval e1 x
eval (Func Cos e1)   x = P.cos $ eval e1 x


