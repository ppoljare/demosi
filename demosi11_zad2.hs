import Data.Char

data Polynom a = Poly [a]
  deriving (Read, Eq)

----- a) -----
showPoly' :: (Show a, Ord a, Num a) => [(a,Int)] -> String
showPoly' [] = ""
showPoly' ((a,i):xs)
  | a >= 0 = "+" ++ x ++ showPoly' xs
  | otherwise = x ++ showPoly' xs
  where x = show a ++ "x^" ++ show i

showPoly :: (Show a, Ord a, Num a) => [(a,Int)] -> String
showPoly [] = ""
showPoly ((a,_):[]) = show a ++ "x^0"
showPoly ((a,i):xs) = show a ++ "x^" ++ show i ++ showPoly' xs

instance (Show a, Ord a, Num a) => Show (Polynom a) where
  show (Poly xs) = showPoly $ reverse $ zip xs [0..]

----- b) -----
splitSign' :: String -> String -> [String]
splitSign' [] acc = reverse acc : []
splitSign' (x:xs) acc
  | x=='+' = reverse acc : splitSign' xs []
  | x=='-' = reverse acc : splitSign' xs "-"
  | otherwise = splitSign' xs (x:acc)

splitSign :: String -> [String]
splitSign ('-':xs) = splitSign' xs "-"
splitSign xs = splitSign' xs []

strToInt :: String -> Int
strToInt ('-':xs) = - strToInt xs
strToInt xs = foldl (\acc x -> acc*10 + cti x) 0 xs
  where cti x = ord x - ord '0'

strToPoly :: String -> Polynom Int
strToPoly xs = Poly $ reverse $ map strToInt coeffStr
  where coeffStr = map (takeWhile (/='x')) (splitSign xs)

----- c) -----
polySum :: Num a => [a] -> [a] -> [a]
polySum [] ys = ys
polySum xs [] = xs
polySum (x:xs) (y:ys) = x+y : polySum xs ys

(.+) :: Num a => Polynom a -> Polynom a -> Polynom a
(.+) (Poly a) (Poly b) = Poly $ polySum a b

(.-) :: Num a => Polynom a -> Polynom a -> Polynom a
(.-) x@(Poly _) (Poly b) = x .+ (Poly minusb)
  where minusb = map (*(-1)) b

polyMult :: Num a => (a,Int) -> [a] -> [a]
polyMult (a, i) ys = (replicate i 0) ++ (map (*a) ys)

(.*) :: Num a => Polynom a -> Polynom a -> Polynom a
(.*) (Poly a) (Poly b) = Poly (foldl polySum [] res)
  where res = map (flip polyMult b) (zip a [0..])
  -- where res = map (\(x,i) -> flip polyMult b (x,i))
  -- where res = map (\(x,i) -> polyMult (x,i) b)

----- d) -----
derive :: Polynom Int -> Polynom Int
derive (Poly (x:[])) = Poly [0]
derive (Poly (x:xs)) = Poly (zipWith (*) xs [1..])
