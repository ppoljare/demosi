-- zad 1
---- vrati uređeni par (p,n)
---- p - el. liste s parnim indeksom
---- n - el. liste s neparnim indeksom
split :: [a] -> [a]
split [] = []
split (x:[]) = [x]
split (x:_:xs) = x : split xs

split_even :: [a] -> ([a], [a])
split_even xs = (split xs, split $ tail xs)

-- zad 2
---- provjeri je li string palindrom
is_pal :: String -> Bool
is_pal xs = xs == reverse xs

-- zad 3
---- vrati listu parcijalnih suma
pref_sums' :: [Int] -> Int -> [Int]
pref_sums' [] _ = []
pref_sums' (x:xs) acc = new_acc : pref_sums' xs new_acc
  where new_acc = acc + x

pref_sums :: [Int] -> [Int]
pref_sums xs = pref_sums' xs 0

-- zad 4
---- provjeri je li broj prost
(//) = div
(%%) = mod

------ rekurzijom
is_prime_rec' :: Int -> Int -> Bool
is_prime_rec' _ 1 = True
is_prime_rec' n i = (n %% i /= 0) && is_prime_rec' n (i-1)

is_prime_rec :: Int -> Bool
is_prime_rec 1 = False
is_prime_rec n = is_prime_rec' n ((n+1)//2)

------ komprehenzijom liste
is_prime :: Int -> Bool
is_prime 1 = False
is_prime n = [i | i <- [1..n], n %% i == 0] == [1,n]

-- zad 5
---- generiraj sve proste brojeve <= n
primes :: Int -> [Int]
primes n = [i | i <- [1..n], is_prime i]

-- zad 6
---- podijeli string na riječi
to_words' :: String -> String -> [String]
to_words' [] acc = [reverse acc]
to_words' (x:xs) acc
  | x==' ' = reverse acc : to_words' xs []
  | otherwise = to_words' xs (x:acc)

to_words :: String -> [String]
to_words xs = to_words' xs []

-- zad 7
---- vrati listu djelitelja od n
divisors :: Int -> [Int]
divisors n = [i | i <- [1..n], n%%i==0]

-- zad 8
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort m ++ x : quicksort v
  where
    m = [e | e <- xs, e <= x]
    v = [e | e <- xs, e > x]

-- zad 9
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | y < x = y : merge (x:xs) ys
  | otherwise = x : merge xs (y:ys)

-- zad 10
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort (x:[]) = [x]
mergeSort xs = merge (mergeSort l) (mergeSort r)
  where
    n = (length xs) // 2
    l = take n xs
    r = drop n xs

-- zad 11
---- rastavi broj na proste faktore
factor' :: Int -> [Int] -> [Int]
factor' 1 _ = []
factor' n s@(x:xs)
  | n==x = [x]
  | n%%x == 0 = x : factor' (n//x) s
  | otherwise = factor' n xs

factor :: Int -> [Int]
factor n = factor' n (primes n)

-- zad 12
---- pretvori string u palindrom (najkraći mogući)
pal :: String -> String -> String
pal (x:[]) acc = acc
pal s@(x:xs) acc
  | is_pal s = acc
  | otherwise = pal xs (x:acc)

palindromize :: String -> String
palindromize [] = []
palindromize s = s ++ pal s []

