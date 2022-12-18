-- zad 1
(//) = div
(%%) = mod

-- zad 2

-- zad 3
data List a = Elem a (List a) | Null
  deriving (Show, Read)

--h2l :: [a] -> List a

--l2h :: List a -> [a]

{-
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
-}

--lmap :: (a -> b) -> List a -> List b

{-
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x = x : next
  | otherwise = next
  where next = filter p xs
-}

--lfilter :: (a -> Bool) -> List a -> List a

{-
(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)
-}

--(.++) :: List a -> List a -> List a

{-
qs :: Ord a => [a] -> [a]
qs [] = []
qs (x:xs) = qs m ++ x : qs v
  where
    m = filter (<=x) xs
    v = filter (>x) xs
-}

--qs :: Ord a => List a -> List a

{-
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
-}

--lfoldl :: (a -> b -> a) -> a -> List b -> a

{-
foldr :: (b -> a -> a) -> a -> [b] -> a
foldr _ acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)
-}

--lfoldr :: (b -> a -> a) -> a -> List b -> a

{-
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ x@(e:[]) = x
intersperse n (x:xs) = x : n : ys
  where ys = intersperse n xs
-}

--lintersperse :: a -> List a -> List a

{-
len :: [a] -> Int
len xs = foldr (\_ acc -> acc+1) 0 xs
-}

--llen :: List a -> Int

{-
bubble :: Ord a => Int -> [a] -> [a]
bubble 1 xs = xs
bubble k (x:y:ys)
  | x > y = y : bubble (k-1) (x:ys)
  | otherwise = x : bubble (k-1) (y:ys)
-}

--bubble :: Ord a => Int -> List a -> List a

{-
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = foldr bubble xs [1..n]
  where n = length xs
-}

--bubbleSort :: Ord a => List a -> List a

{-
insert :: Ord a => a -> [a] -> [a]
insert n [] = n : []
insert n (x:xs)
  | n <= x = n : x : xs
  | otherwise = x : insert n xs
-}

--linsert :: Ord a => a -> List a -> List a

{-
insertionSort :: Ord a => [a] -> [a]
insertionSort xs = foldl (flip insert) [] xs
-}

--insertionSort :: Ord a => List a -> List a

-- zad 4
data Term =
  Ttrue | Tfalse | Not Term
  | And Term Term | Or Term Term
    deriving (Read, Show)
