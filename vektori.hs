data Vector a = Vec [a]
  deriving (Show, Read)

-- zbrajanje
(.+) :: (Num a) => Vector a -> Vector a -> Vector a
(.+) (Vec xs) (Vec ys) = Vec $ zipWith (+) xs ys

-- oduzimanje
(.-) :: (Num a) => Vector a -> Vector a -> Vector a
(.-) (Vec xs) (Vec ys) = Vec $ zipWith (-) xs ys

-- skalarni produkt
(.*) :: (Num a) => Vector a -> Vector a -> a
(.*) (Vec xs) (Vec ys) = sum $ zipWith (*) xs ys

-- mnozenje skalarom
(.*.) :: (Num a) => a -> Vector a -> Vector a
(.*.) a (Vec xs) = Vec (map (a*) xs)

-- norma
norm :: (Floating a) => Vector a -> a
norm v = sqrt $ v .* v