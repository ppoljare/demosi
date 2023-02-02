import Data.Char

data Expr = 
  Num Int | Minus Expr
  | Add Expr Expr | Sub Expr Expr | Mult Expr Expr
  deriving (Read)

----- a) -----
showExpr :: Expr -> String
showExpr (Num x)
  | x < 0 = "(" ++ show x ++ ")"
  | otherwise = show x
showExpr (Minus x) = "(-" ++ showExpr x ++ ")"
showExpr (Add x y) = "(" ++ showExpr x ++ "+" ++ showExpr y ++ ")"
showExpr (Sub x y) = "(" ++ showExpr x ++ "-" ++ showExpr y ++ ")"
showExpr (Mult x y) = "(" ++ showExpr x ++ "*" ++ showExpr y ++ ")"

instance Show Expr where
  show (Num x) = show x
  show (Minus x) = "-" ++ showExpr x
  show (Add x y) = showExpr x ++ "+" ++ showExpr y
  show (Sub x y) = showExpr x ++ "-" ++ showExpr y
  show (Mult x y) = showExpr x ++ "*" ++ showExpr y

----- b) -----
eval :: Expr -> Int
eval (Num x) = x
eval (Minus x) = - eval x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mult x y) = eval x * eval y

----- c) -----
splitOps' :: String -> String -> Int -> [String]
splitOps' [] acc i = [reverse acc]
splitOps' (x:xs) acc i
  | and [i==0, elem x "+-*"] = rev_acc : [x] : next [] i
  | and [i==0, x=='('] = rev_acc : next [] 1
  | and [i==1, x==')'] = rev_acc : next [] 0
  | x=='(' = next (x:acc) (i+1)
  | x==')' = next (x:acc) (i-1)
  | otherwise = next (x:acc) i
  where
    rev_acc = reverse acc
    next = splitOps' xs

splitOps :: String -> [String]
splitOps xs = filter (\x -> length x > 0) xs'
  where xs' = splitOps' xs [] 0

----- d) -----
stoi :: String -> Int
stoi ('-':xs) = - stoi xs
stoi xs = foldl (\acc x -> acc*10 + ctoi x) 0 xs
  where ctoi x = ord x - ord '0'

strToExpr' :: [String] -> Expr
strToExpr' (x:[]) = Num $ stoi x
strToExpr' ("-":x:[]) = Minus $ strToExpr x
strToExpr' (x:"+":y:[]) = Add (strToExpr x) (strToExpr y)
strToExpr' (x:"-":y:[]) = Sub (strToExpr x) (strToExpr y)
strToExpr' (x:"*":y:[]) = Mult (strToExpr x) (strToExpr y)

strToExpr :: String -> Expr
strToExpr xs = strToExpr' $ splitOps xs
