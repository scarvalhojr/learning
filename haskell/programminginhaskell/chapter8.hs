
type Pos = (Int, Int)

data Move = North | South | East | West deriving Show

move :: Move -> Pos -> Pos
move North (x,y) = (x, y + 1)
move South (x,y) = (x, y - 1)
move East  (x,y) = (x + 1, y)
move West  (x,y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves []     p = p
moves (m:ms) p = moves ms (move m p)

double :: (Int, Int) -> (Int, Int)
double (x,y) = (2 * x, 2 * y)

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add n m = int2nat (nat2int n + nat2int m)

add' :: Nat -> Nat -> Nat
add' Zero     n = n
add' (Succ m) n = Succ (add' m n)

data List a = Nil | Cons a (List a) deriving Show

len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf m)     = m == x
occurs x (Node l m r) = m == x || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l m r) = flatten l ++ [m] ++ flatten r

occurs' :: Eq a => a -> Tree a -> Bool
occurs' x (Leaf m) = m == x
occurs' x (Node l m r)
    | m == x    = True
    | m >  x    = occurs' x l
    | otherwise = occurs' x r
