
-- Exercise 2

-- outermost evaluation (3 steps)
--
-- fst (1 + 2, 2 + 3)
--   => apply fst
-- (1 + 2)
--   => apply +
-- 3

-- innermost evaluation (4 steps)
--
-- fst (1 + 2, 2 + 3)
--   => apply +
-- fst (3, 2 + 3)
--   => apply +
-- fst (3, 5)
--   => apply fst
-- 3

-- Exercice 3

-- mult = \x -> (\y -> x * y)

-- mult 3 4
--   => apply mult
-- (\x -> (\y -> x * y)) 3 4
--   => apply lambda
-- (\y -> 3 * y) 4
--   => apply lambda
-- 3 * 4
--   => apply *
-- 12

-- Exercise 4

fibs :: [Integer]
fibs = fibs' 0 1
  where fibs' :: Integer -> Integer -> [Integer]
        fibs' x y = x : fibs' y (x + y)

-- using zip and tail???

-- Exercise 5

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

repeatT :: a -> Tree a
repeatT x = Node (repeatT x) x (repeatT x)

takeT :: Int -> Tree a -> [a]
takeT _ Leaf = []
takeT n (Node l e r)
  | n <= 0               = []
  | otherwise            = e : lx ++ takeT (n - 1 - lL) r
  where lx = takeT (n - 1) l
        lL = length lx

replicateT :: Int -> a -> [a]
replicateT n = takeT n . repeatT

-- Exercise 6

sqroot :: Double -> Double
sqroot n = fst (last c)
  where a = iterate (\x -> (x + n / x) / 2) 1.0
        b = zipWith (\ n p -> (n, n - p)) (tail a) a
        c = takeWhile ((<=) 0.00001 . abs. snd) b

sqroot' :: Double -> Double
sqroot' = approxSqroot 1.0

approxSqroot :: Double -> Double -> Double
approxSqroot approx n
  | abs (nxt - approx) > 0.00001  = approxSqroot nxt n
  | otherwise                     = nxt
  where nxt = (approx + n / approx) / 2.0
