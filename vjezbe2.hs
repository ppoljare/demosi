-- Zadatak 1
(%%) = mod

is_even :: Int -> Bool
is_even n = n %% 2 == 0

is_even_rec' :: Int -> Bool
is_even_rec' 0 = True
is_even_rec' 1 = False
is_even_rec' n = is_even_rec' (n-2)

is_even_rec :: Int -> Bool
is_even_rec n = is_even_rec' $ abs n

-- Zadatak 2
pwr2 :: Int -> Int
pwr2 0 = 1
pwr2 n = 2 * pwr2 (n-1)

-- Zadatak 3
geo2 :: Int -> Int
geo2 0 = 1
geo2 n = 1 + 2 * geo2 (n-1)

-- Zadatak 4
apply :: (t -> t) -> t -> Int -> t
apply _ a 0 = a
apply f a n = f (apply f a (n-1))

-- Zadatak 5
pwr :: Num a => a -> Int -> a
pwr x n = apply (*x) 1 n

pwr_rec :: Num a => a -> Int -> a
pwr_rec _ 0 = 1
pwr_rec x n = x * pwr_rec x (n-1)

-- Zadatak 6
geo :: Int -> Int -> Int
geo b 0 = 1
geo b n = 1 + b * geo b (n-1)

-- Zadatak 7
indx :: Ord a => [a] -> (a -> a -> Bool) -> a -> Int -> Int -> Int
indx [] _ _ res _ = res
indx (x:xs) comp m res i =
    if comp x m then
        indx xs comp x i (i+1)
    else
        indx xs comp m res (i+1)

min_indx :: Ord a => [a] -> Int
min_indx [] = -1
min_indx s = indx s (<) (head s) 0 0

max_indx :: Ord a => [a] -> Int
max_indx [] = -1
max_indx s = indx s (>) (head s) 0 0

-- Zadatak 8
dot_poly :: [Int] -> [Int] -> Int
dot_poly (x:xs) (y:ys) = x*y + dot_poly xs ys
dot_poly _ _ = 0

-- Zadatak 9
apply_all :: (t -> t) -> t -> Int -> [t]
apply_all _ a 0 = [a]
apply_all f a n = a : apply_all f (f a) (n-1)

-- Zadatak 10
sqrt2' :: Int -> Int -> Int
sqrt2' n x =
    if x^2 > n then
        x-1
    else
        sqrt2' n (x+1)

sqrt2 :: Int -> Int
sqrt2 n = sqrt2' n 0

-- Zadatak 11
poly_eval :: [Int] -> Int -> Int
poly_eval [] _ = 0
poly_eval s x = dot_poly s (apply_all (*x) 1 (length s))

-- Zadatak 12
lg' :: Int -> Int -> Int
lg' n res
  | pwr2 res > n = res-1
  | otherwise = lg' n (res+1)

lg :: Int -> Int
lg n = lg' n 0

main :: IO()
main = do
    --print $ is_even 6
    --print $ is_even 19
    --print $ is_even_rec 6
    --print $ is_even_rec 19
    --print $ pwr2 5
    --print $ geo2 5
    --print $ apply (*2) 1 5
    --print $ pwr 1.2 2
    --print $ pwr_rec 1.2 2
    --print $ geo 2 5
    --print $ min_indx [1, 5, 7, 0, 7, 0]
    --print $ max_indx [1, 5, 7, 0, 7, 0]
    --print $ dot_poly [1,2,3] [4,5,6,7]
    --print $ apply_all (*2) 1 5
    print $ sqrt2 5
    --print $ poly_eval [1, 2, 3, 4] 2
    --print $ lg 2
    