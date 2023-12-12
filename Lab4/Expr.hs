module Expr where

  
import Prelude hiding (sin, cos)
import qualified Prelude as P (sin, cos)
import Test.QuickCheck

import Parsing


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

-- | Type declaration for Expr and types it contains.

x :: Expr
x = X 

num :: Double -> Expr
num = Num

add,mul :: Expr -> Expr -> Expr
add = Opr Add
mul = Opr Mul

sin,cos :: Expr -> Expr 
sin = Func Sin 
cos = Func Cos 

-- | Function for calcultating number of functions and operators in an Expr.

size :: Expr -> Int
size (Opr _ e1 e2) = 1 + size e1 + size e2
size (Func _ e1)   = 1 + size e1
size _             = 0

-- | Function for converting an Expr to how it would be written as an equation with type String.

showExpr :: Expr -> String
showExpr (Opr Mul (Num (-1)) e) = "-" ++ showFactor e
showExpr (Opr Mul e (Num (-1))) = "-" ++ showFactor e
showExpr (Opr Mul e1 e2)        = showFactor e1 ++ "*" ++ showFactor e2
showExpr (Opr Add e1 e2)        = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Func f e)             = case f of Sin -> "sin"; Cos -> "cos" 
                                      ++ "(" ++ showExpr e ++ ")"  
showExpr X                      = "x"
showExpr (Num n)                = show n

-- | Factors has to be handled differently because of arithmetic priority rules.
showFactor :: Expr -> String
showFactor (Opr Add e1 e2) = "("++ showExpr (Opr Add e1 e2) ++")"
showFactor e               = showExpr e

-- | Given an Expression and a value for X, it evaluates the result of the expression.
eval :: Expr -> Double -> Double
eval (Num n) x         = n
eval X x               = x
eval (Opr opr e1 e2) x = case opr of Add -> eval e1 x + eval e2 x
                                     Mul -> eval e1 x * eval e2 x
eval (Func fun e) x    = case fun of Sin -> P.sin $ eval e x
                                     Cos -> P.cos $ eval e x


-- | Function that, given a string, tries to interpret the string as an expression
-- | and returns Just of that expression if it succeeds.
-- | Otherwise, Nothing will be returned
readExpr :: String -> Maybe Expr
readExpr s = case parse expr s'' of 
    Just(e, _) -> Just e
    Nothing -> Nothing
  where
    s' = filter (/=' ') s
    s'' = repl s'
    repl [] = ""
    repl (s:'-':ss) | s /= 'e' = case s of '-' -> "(-1)*"; _ -> [s]; ++  "(-1)*" ++ repl ss 
                    | otherwise = "e-" ++ repl ss
    repl ('-':ss) = "(-1)*" ++ repl ss
    repl (s:ss) = s : repl ss


-- Expression parser
expr, term, factor :: Parser Expr
expr   = foldl1 add <$> chain term (char '+')
term = foldl1 mul <$> chain factor (char '*')
factor = Num <$> number 
      <|> xParse 
      <|> trig "sin"
      <|> trig "cos"
      <|> char '(' *> expr <* char ')' 

-- | Parser for trigonometric expressions 
trig s = (case s of "sin" -> sin; "cos" -> cos) 
        <$> (string s *> factor)
  where 
    -- | Parses the three first chars of a string, there is probably a better way to do this..
    string :: String -> Parser String
    string (s1:s2:s3:ss)=  
      do 
        char s1
        char s2
        char s3 
        return [s1,s2,s3]

-- | Parse X
xParse :: Parser Expr
xParse = do 
  char 'x' 
  return X


-- | Parses a negative or a positive number (Different ca)
number :: Parser Double
number = negNumber <|> posNumber

-- | Parses a negative number
negNumber :: Parser Double
negNumber = do 
  char '-'
  num <- number
  return (-num)

-- | Parses a positive number
posNumber :: Parser Double
posNumber = parseDouble <|> parseInt

-- | Parses an Int
parseInt :: Parser Double
parseInt =  read <$> oneOrMore digit

-- | Parses a Double 
parseDouble :: Parser Double
parseDouble = do
    whole <- oneOrMore digit
    char '.'
    fract <- oneOrMore digit
    sien <- zeroOrMore parseSientific
    pure $ read $ whole <> "." <> fract <> (if sien /= [] then head sien else "")

-- | Parse the sientific notation ending of some doubles
parseSientific :: Parser String
parseSientific = do
  e <- char 'e'
  neg <- zeroOrMore $ char '-'
  n <- oneOrMore digit
  pure $ "e" <> neg <> n
  
-- | Property of a parsed expression. An Expr that has been shown, then parsed, must evaluate to the same value 
-- | (taking floating point error into account) as it evaluates to before.
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = abs (eval e 5 - eval rwe 5) <= 0.0000001 
  where (Just rwe) = readExpr $ showExpr e

-- | Generator for an expression using s as the size argument
arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1, return X), (1, rNum), 
                          (s, rBin), (s, rTrig)]
      where
        range = 5
        rNum  = Num <$> choose(-range,range)
        rBin  = do
          op <- elements [Add,Mul]
          let s' = s `div` 2
          e1 <- arbExpr s'
          e2 <- arbExpr s'
          return $ Opr op e1 e2 
        rTrig = do
          tri <- elements [Cos, Sin]
          let s' = s `div` 2
          e <- arbExpr s'
          return $ Func tri e 
        
instance Arbitrary Expr where 
  arbitrary = sized arbExpr



-- | Simplifys an expression until it can not be simplfied any more 
simplify :: Expr -> Expr
simplify e  | e == simplify' e = e 
             | otherwise = simplify $ simplify' e   
  where
    simplify' :: Expr -> Expr
    simplify' (Opr Mul (Num 0.0) _) = Num 0.0 
    simplify' (Opr Mul _ (Num 0.0)) = Num 0.0 
    simplify' (Opr Mul (Num (-1)) (Num (-1))) = Num 1 
    simplify' (Opr Mul (Num (1)) (Num (1))) = Num 1 
    simplify' (Opr Mul (Num (-1)) (Num (1))) = Num (-1) 
    simplify' (Opr Mul (Num (1)) (Num (-1))) = Num (-1) 
    simplify' (Opr Mul (Num (1)) e) = simplify' e 
    simplify' (Opr Mul e (Num (1))) = simplify' e 
    simplify' (Opr Add (Num 0.0) e) = simplify' e
    simplify' (Opr Add e (Num 0.0)) = simplify' e
    simplify' (Opr Add (Num n1) (Num n2)) = Num (n1 + n2)
    simplify' (Opr Add X X) = Opr Mul (Num 2) X
    simplify' (Opr Add (Opr Mul (Num n) X) X) = mul (Num (n+1)) X
    simplify' (Opr Add X (Opr Mul (Num n) X)) = mul (Num (n+1)) X
    simplify' (Opr Add e1 e2) = add (simplify' e1) (simplify' e2)
    simplify' (Opr Mul e1 e2) = mul (simplify' e1) (simplify' e2)
    simplify' (Func Sin e) = Func Sin (simplify' e)
    simplify' (Func Cos e) = Func Cos (simplify' e)
    simplify' (Num n) = Num n
    simplify' X = X

-- | Property for simplyfied expressions. 
-- | They must evaluate to the same value as before it was simplyfied.
prop_evalsimplify :: Expr -> Double -> Bool
prop_evalsimplify e n = eval e n == eval (simplify e) n


-- | Differentiates the expression (with respect to x).
differentiate :: Expr -> Expr
differentiate e = simplify $ differentiate' e
  where
    differentiate' (Num n)             = Num 0.0
    differentiate' X                 = Num 1
    differentiate' (Opr Mul (Num n) X) = Num n
    differentiate' (Opr Mul X (Num n)) = Num n
    differentiate' (Opr Mul X X)       = mul (Num 2) X
    differentiate' (Func Sin e)        = cos e
    differentiate' (Func Cos e)        = mul (Num (-1)) (sin e)
    differentiate' (Opr Add e1 e2)     = add (differentiate' e1) (differentiate' e2)
    differentiate' (Opr Mul e1 e2)     = add (mul (differentiate' e1) e2) 
                                        (mul e1 (differentiate' e2))
