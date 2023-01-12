-- demosi 10

-- zad 3
data List a = Elem a (List a) | Null
  deriving (Show, Read)

h2l :: [a] -> List a
h2l [] = Null
h2l (x:xs) = Elem x (h2l xs)

l2h :: List a -> [a]
l2h Null = []
l2h (Elem x xs) = x : l2h xs

{-
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
-}
lfoldl :: (a -> b -> a) -> a -> List b -> a
lfoldl _ acc Null = acc
lfoldl f acc (Elem x xs) = lfoldl f (f acc x) xs

{-
foldr :: (b -> a -> a) -> a -> [b] -> a
foldr _ acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)
-}
lfoldr :: (b -> a -> a) -> a -> List b -> a
lfoldr _ acc Null = acc
lfoldr f acc (Elem x xs) = f x (lfoldr f acc xs)

{-
len :: [a] -> Int
len xs = foldr (\_ acc -> acc+1) 0 xs
-}
llen :: List a -> Int
llen xs = lfoldr (\_ acc -> acc+1) 0 xs

{-
bubble :: Ord a => Int -> [a] -> [a]
bubble 1 xs = xs
bubble k (x:y:ys)
  | x > y = y : bubble (k-1) (x:ys)
  | otherwise = x : bubble (k-1) (y:ys)
-}
bubble :: Ord a => Int -> List a -> List a
bubble 1 xs = xs
bubble k (Elem x (Elem y ys))
  | x > y = Elem y (bubble (k-1) (Elem x ys))
  | otherwise = Elem x (bubble (k-1) (Elem y ys))

{-
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = foldr bubble xs [1..n]
  where n = length xs
-}
bubbleSort :: Ord a => List a -> List a
bubbleSort xs = lfoldr bubble xs (h2l [1..n])
  where n = llen xs

{-
insert :: Ord a => a -> [a] -> [a]
insert n [] = n : []
insert n (x:xs)
  | n <= x = n : x : xs
  | otherwise = x : insert n xs
-}
linsert :: Ord a => a -> List a -> List a
linsert n Null = Elem n Null
linsert n (Elem x xs)
  | n <= x = Elem n (Elem x xs)
  | otherwise = Elem x (linsert n xs)

{-
insertionSort :: Ord a => [a] -> [a]
insertionSort xs = foldl (flip insert) [] xs
-}
insertionSort :: Ord a => List a -> List a
insertionSort xs = lfoldl (flip linsert) Null xs