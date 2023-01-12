-- Boolean
data Boolean = T | F

instance Show Boolean where
  show T = "T"
  show F = "F"

instance Eq Boolean where
  (==) T T = True
  (==) F F = True
  (==) _ _ = False
  (/=) x y = not $ x==y

-- List a
data List a = Elem a (List a) | Null

printList :: (Show a) => List a -> String
printList Null = ""
printList (Elem x Null) = show x
printList (Elem x xs) = show x ++ "," ++ printList xs

instance (Show a) => Show (List a) where 
  show Null = "[]"
  show xs = "[" ++ printList xs ++ "]"

instance (Eq a) => Eq (List a) where
  (==) Null Null = True
  (==) (Elem x xs) (Elem y ys) = x==y && xs==ys
  (==) _ _ = False
  (/=) xs ys = not $ xs==ys

-- Tree a
data Tree a = Node (Tree a) a (Tree a) | TNull

instance (Show a) => Show (Tree a) where
  show TNull = ""
  show (Node TNull x TNull) = show x
  show (Node l@(Node _ _ _) x TNull) = "(" ++ show l ++ ")--" ++ show x
  show (Node TNull x r@(Node _ _ _)) = show x ++ "--(" ++ show r ++ ")"
  show (Node l x r) = "(" ++ show l ++ ")--" ++ show x ++ "--(" ++ show r ++ ")"

instance (Eq a) => Eq (Tree a) where
  (==) TNull TNull = True
  (==) (Node a b c) (Node d e f) = and [a==d, b==e, c==f]
  (==) _ _ = False
  (/=) x y = not $ x==y