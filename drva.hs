data Tree a = Node (Tree a) a (Tree a) | Null
  deriving (Show, Eq)

l2BST :: (Ord a) => [a] -> Tree a
l2BST xs = foldl (flip insertBST) Null xs

preorderT :: Tree a -> [a]
preorderT Null = []
preorderT (Node lt x rt) = [x] ++ preorderT lt ++ preorderT rt

insertBST :: (Ord a) => a -> Tree a -> Tree a
insertBST x Null = Node Null x Null
insertBST x (Node lt y rt)
  | x < y      = Node (insertBST x lt) y rt
  | otherwise  = Node lt y (insertBST x rt)

minBST :: (Ord a) => Tree a -> a
minBST (Node Null x _) = x
minBST (Node lt _ _) = minBST lt

searchBST :: (Ord a) => a -> Tree a -> Tree a
searchBST _ Null = Null
searchBST x yt@(Node lt y rt)
  | x == y    = yt
  | x < y     = searchBST x lt
  | otherwise = searchBST x rt

nextBST :: (Ord a) => a -> Tree a -> Tree a
nextBST x t
  | f==[] = Null
  | otherwise = searchBST x' t
  where
    f = filter (>x) (preorderT t)
    x' = minimum f

deleteBST' :: (Ord a) => Tree a -> Tree a
deleteBST' (Node lt x Null) = lt
deleteBST' (Node Null x rt) = rt
deleteBST' (Node lt x rt) = Node lt m rt'
  where
    m = minBST rt
    rt' = deleteBST m rt

deleteBST :: (Ord a) => a -> Tree a -> Tree a
deleteBST x Null = Null
deleteBST x t@(Node lt y rt)
  | x < y = Node (deleteBST x lt) y rt
  | x > y = Node lt y (deleteBST x rt)
  | otherwise = deleteBST' t
