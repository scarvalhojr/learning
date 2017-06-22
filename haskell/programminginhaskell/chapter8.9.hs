
-- Exercise 1

data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _     = Zero
mult _    Zero  = Zero
mult (Succ x) y = add y (mult x y)

-- Exercise 2

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf m)     = m == x
occurs x (Node l m r) = case compare x m of
                            LT -> occurs x l
                            EQ -> True
                            GT -> occurs x r

-- Exercise 3

data BinTree a = BinLeaf a | BinNode (BinTree a) (BinTree a) deriving Show

leaves :: BinTree a -> Int
leaves (BinLeaf _)   = 1
leaves (BinNode x y) = (leaves x) + (leaves y)

balanced :: BinTree a -> Bool
balanced (BinLeaf _)   = True
balanced (BinNode x y) = abs(leaves x - leaves y) <= 1
                         && balanced x && balanced y

-- Exercise 4

halve :: [a] -> ([a], [a])
halve xs = splitAt ((length xs) `div` 2) xs

balance :: [a] -> BinTree a
balance [x] = BinLeaf x
balance xs  = BinNode (balance l) (balance r)
    where (l, r) = halve xs

-- Exercise 5

data Expr = Val Int | Add Expr Expr deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n)   = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

e1 :: Expr
e1 = Add (Val 5) (Add (Val 3) (Val 2))

e2 :: Expr
e2 = Add (Add (Val 1) (Val 4)) (Add (Val 3) (Val 2))

-- Exercise 6

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)

-- Exercise 7

-- instance Eq a => Eq (Maybe a) where

-- instance Eq a => Eq [a] where
